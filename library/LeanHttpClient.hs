-- |
-- A lightweight DSL providing for declarative definition
-- of HTTP requests and parsers.
--
-- The main premise of this library is that requests are
-- by nature coupled with expectations on the possible responses.
-- It builds upon the ideas of the Elm \"http\" library
-- (https://package.elm-lang.org/packages/elm/http/latest).
module LeanHttpClient
  ( -- * Execution
    Err (..),
    runSession,
    runSessionOnGlobalManager,

    -- * Session
    Session,
    performGet,
    performPost,
    performPut,

    -- ** Host
    Host,
    textHost,

    -- ** Path
    Path,
    textPath,

    -- ** Request headers
    RequestHeaders,
    requestHeader,

    -- * Response parsing
    ResponseParser,
    extractHeaders,
    parseJsonBody,

    -- ** Response headers parsing
    ResponseHeaders,
    lookupInResponseHeaders,

    -- * Utils
    assemblePathString,
    assembleQueryString,
  )
where

import qualified AesonValueParser as Avp
import qualified Attoparsec.Data as AttoparsecData
import qualified ByteString.StrictBuilder as BsBuilder
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser
import qualified Data.Attoparsec.ByteString as BsAttoparsec
import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as Ci
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Distillery.Extractor (Extractor (..))
import qualified Distillery.Extractor as Extractor
import LeanHttpClient.Prelude hiding (get, put)
import qualified LeanHttpClient.Serialization as Serialization
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS
import qualified PtrPoker.Write as Serialization

-------------------------

data Err
  = -- | Connection timed out.
    TimeoutErr
  | -- | Any other network-related problem.
    NetworkErr Text
  | -- | Unexpected response.
    UnexpectedResponseErr Text
  deriving (Show)

newtype Session a
  = Session (Client.Manager -> IO (Either Err a))
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadError Err)
    via (ExceptT Err (ReaderT Client.Manager IO))

newtype ResponseParser a
  = ResponseParser (Client.Response Client.BodyReader -> IO (Either Err a))
  deriving
    (Functor, Applicative, Monad, MonadError Err)
    via (ExceptT Err (ReaderT (Client.Response Client.BodyReader) IO))

newtype ResponseHeaders
  = -- |
    --  A map from lower-case header names to their values.
    --
    --  Text is used instead of ByteString because it's a human-readable information,
    --  not a byte array.
    ResponseHeaders (HashMap Text Text)

data QueryParam
  = FlagQueryParam Text
  | AssocQueryParam Text Text

-------------------------

runSession :: Session a -> Client.Manager -> IO (Either Err a)
runSession =
  coerce

runSessionOnGlobalManager :: Session a -> IO (Either Err a)
runSessionOnGlobalManager session =
  do
    manager <- Network.HTTP.Client.TLS.getGlobalManager
    runSession session manager

-------------------------

performGet ::
  DiffTime ->
  Int ->
  Bool ->
  Host ->
  Maybe Word16 ->
  Path ->
  [(Text, Text)] ->
  RequestHeaders ->
  ResponseParser a ->
  Session a
performGet timeout maxRedirects secure host portMb path query requestHeaders =
  performRequest
    (assembleRawRequest timeout maxRedirects "GET" secure host portMb path query requestHeaders mempty)

performPost ::
  DiffTime ->
  Int ->
  Bool ->
  Host ->
  Maybe Word16 ->
  Path ->
  [(Text, Text)] ->
  RequestHeaders ->
  ByteString ->
  ResponseParser a ->
  Session a
performPost timeout maxRedirects secure host portMb path query requestHeaders requestBody =
  performRequest
    (assembleRawRequest timeout maxRedirects "POST" secure host portMb path query requestHeaders requestBody)

performPut ::
  DiffTime ->
  Int ->
  Bool ->
  Host ->
  Maybe Word16 ->
  Path ->
  [(Text, Text)] ->
  RequestHeaders ->
  ByteString ->
  ResponseParser a ->
  Session a
performPut timeout maxRedirects secure host portMb path query requestHeaders requestBody =
  performRequest
    (assembleRawRequest timeout maxRedirects "PUT" secure host portMb path query requestHeaders requestBody)

performRequest :: Client.Request -> ResponseParser a -> Session a
performRequest request (ResponseParser parseResponse) =
  Session $ \manager ->
    catch (Client.withResponse request manager parseResponse) (return . Left . normalizeRawException)

-- * HTTP-Client Assemblage

-------------------------

assemblePathString :: Path -> ByteString
assemblePathString (Path seq) =
  if Seq.null seq
    then "/"
    else
      Serialization.toStrictByteString $
        foldMap (mappend "/" . Serialization.percentEncodedPathSegmentText) seq

-- |
-- Create a query string with all those ampersands and percent-encoding
-- from textual key-value pairs.
--
-- If value in a pair is empty,
-- then no @=@ will be inserted.
assembleQueryString :: Foldable f => f (Text, Text) -> ByteString
assembleQueryString f =
  foldr step finalize f True mempty
  where
    step (k, v) next first mason =
      next False newMason
      where
        newMason =
          if first
            then Serialization.char7 '?' <> param
            else mason <> Serialization.char7 '&' <> param
          where
            param =
              if Text.null v
                then Serialization.percentEncodedQuerySegmentText k
                else
                  Serialization.percentEncodedQuerySegmentText k
                    <> Serialization.char7 '='
                    <> Serialization.percentEncodedQuerySegmentText v
    finalize _ mason =
      Serialization.toStrictByteString mason

assembleRawHeaders :: RequestHeaders -> [(CI ByteString, ByteString)]
assembleRawHeaders (RequestHeaders seq) =
  fmap mapHeader (toList seq)
  where
    mapHeader (name, value) =
      (Ci.mk (Text.encodeUtf8 name), Text.encodeUtf8 value)

assembleRawRequest ::
  DiffTime ->
  Int ->
  ByteString ->
  Bool ->
  Host ->
  Maybe Word16 ->
  Path ->
  [(Text, Text)] ->
  RequestHeaders ->
  ByteString ->
  Client.Request
assembleRawRequest timeout maxRedirects method secure host portMb path query requestHeaders body =
  Client.defaultRequest
    { Client.host =
        coerce host,
      Client.port =
        maybe (bool 80 443 secure) fromIntegral portMb,
      Client.secure =
        secure,
      Client.requestHeaders =
        assembleRawHeaders requestHeaders,
      Client.path =
        assemblePathString path,
      Client.queryString =
        assembleQueryString query,
      Client.requestBody =
        Client.RequestBodyBS body,
      Client.method =
        method,
      Client.redirectCount =
        maxRedirects,
      Client.responseTimeout =
        Client.responseTimeoutMicro (round (timeout * 1000000))
    }

-------------------------

normalizeRawException :: Client.HttpException -> Err
normalizeRawException =
  \case
    Client.HttpExceptionRequest _ a -> case a of
      Client.ConnectionTimeout ->
        TimeoutErr
      Client.ResponseTimeout ->
        TimeoutErr
      err ->
        NetworkErr (fromString (show err))
    Client.InvalidUrlException uri reason ->
      error ("Failed parsing a URI (" <> uri <> "): " <> reason)

extractRawResponse :: Extractor (Client.Response Client.BodyReader) a -> ResponseParser a
extractRawResponse extractor =
  ResponseParser (pure . first UnexpectedResponseErr . Extractor.extract extractor)

-------------------------

-- |
-- Parse headers with extractor.
extractHeaders :: Extractor ResponseHeaders a -> ResponseParser a
extractHeaders extractor =
  extractRawResponse (extractor . lmap Client.responseHeaders normalizeHeaders)
  where
    normalizeHeaders :: Extractor [(CI ByteString, ByteString)] ResponseHeaders
    normalizeHeaders =
      rmap (ResponseHeaders . HashMap.fromList) (traverse' normalizeHeader)
    normalizeHeader :: Extractor (CI ByteString, ByteString) (Text, Text)
    normalizeHeader =
      (,) <$> lmap (Ci.foldedCase . fst) Extractor.decodeUtf8 <*> lmap snd Extractor.decodeUtf8

lookupInResponseHeaders :: Text -> Extractor ResponseHeaders Text
lookupInResponseHeaders =
  lmap coerce . Extractor.lookupInHashMap . Text.toCaseFold

-------------------------

parseResponseBodyBytes :: BsAttoparsec.Parser a -> ResponseParser a
parseResponseBodyBytes parser =
  ResponseParser $ \response ->
    BsAttoparsec.parseWith (Client.responseBody response) parser ""
      & fmap
        ( \case
            BsAttoparsec.Done _ a ->
              Right a
            BsAttoparsec.Fail remainder contexts details ->
              let msg =
                    "Failed parsing bytes in context /"
                      <> Text.intercalate "/" (fmap fromString contexts)
                      <> ": "
                      <> fromString details
                      <> ". "
                      <> "Remainder: "
                      <> fromString (show remainder)
               in Left $ UnexpectedResponseErr $ msg
            BsAttoparsec.Partial _ ->
              Left (UnexpectedResponseErr "Response data interrupted")
        )

extractAesonValue :: Extractor Aeson.Value a -> ResponseParser a
extractAesonValue extractor =
  do
    aesonValue <- parseResponseBodyBytes Data.Aeson.Parser.json
    ResponseParser $ const $ pure $ first UnexpectedResponseErr $ Extractor.extract extractor aesonValue

parseJsonBody :: Avp.Value a -> ResponseParser a
parseJsonBody =
  extractAesonValue . Extractor.parseAesonValue

-- * Host

-------------------------

newtype Host
  = Host ByteString

instance IsString Host where
  fromString =
    textHost . fromString

textHost :: Text -> Host
textHost =
  Host . Serialization.toStrictByteString . Serialization.domain

-- * Path

-------------------------

newtype Path
  = Path (Seq Text)
  deriving (Semigroup, Monoid)

instance IsString Path where
  fromString =
    textPath . fromString

textPath :: Text -> Path
textPath =
  Path . Seq.fromList . Text.split (== '/') . Text.dropAround (== '/')

-- * RequestHeaders

-------------------------

newtype RequestHeaders
  = RequestHeaders (Seq (Text, Text))
  deriving (Semigroup, Monoid)

requestHeader :: Text -> Text -> RequestHeaders
requestHeader name value =
  RequestHeaders (pure (name, value))
