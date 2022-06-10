-- |
-- A lightweight DSL providing for declarative definition
-- of HTTP requests and parsers.
--
-- The main premise of this library is that requests are
-- by nature coupled with expectations on the possible responses.
-- It builds upon the ideas of [the Elm \"http\" library]
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
    deserializeBodyWithCereal,

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
import qualified Data.Serialize as Cereal
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Distillery.Extractor (Extractor (..))
import qualified Distillery.Extractor as Extractor
import LeanHttpClient.Prelude hiding (get, put)
import qualified LeanHttpClient.Serialization as Serialization
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS

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
  = Session (Config -> Client.Manager -> IO (Either Err a))
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadError Err)
    via (ExceptT Err (ReaderT Config (ReaderT Client.Manager IO)))

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

newtype Host
  = Host ByteString

instance IsString Host where
  fromString =
    textHost . fromString

newtype Path
  = Path (Seq Text)
  deriving (Semigroup, Monoid)

instance IsString Path where
  fromString =
    textPath . fromString

newtype RequestHeaders
  = RequestHeaders (Seq (Text, Text))
  deriving (Semigroup, Monoid)

data Config = Config
  { configTimeout :: DiffTime,
    configMaxRedirects :: Int
  }

data Url = Url
  { -- | HTTPS? HTTP otherwise.
    urlSecure :: !Bool,
    -- | Host name.
    urlHost :: !Host,
    -- | Specific port if present. Default port for the protocol otherwise.
    urlPort :: !(Maybe Word16),
    -- | Path at the host.
    urlPath :: !Path,
    -- | Query params.
    urlQuery :: ![(Text, Text)]
  }

data Request a

-------------------------

runSession :: Session a -> Client.Manager -> IO (Either Err a)
runSession =
  error "TODO"

runSessionOnGlobalManager :: Session a -> IO (Either Err a)
runSessionOnGlobalManager session =
  do
    manager <- Network.HTTP.Client.TLS.getGlobalManager
    runSession session manager

-- * Sessions

-------------------------

mapConfig :: (Config -> Config) -> Session a -> Session a
mapConfig mapper (Session run) =
  Session $ run . mapper

-- | Override the timeout setting for the provided session.
overrideTimeout :: DiffTime -> Session a -> Session a
overrideTimeout val =
  mapConfig $ \config -> config {configTimeout = val}

-- | Override the max redirects setting for the provided session.
overrideMaxRedirects :: Int -> Session a -> Session a
overrideMaxRedirects val =
  mapConfig $ \config -> config {configMaxRedirects = val}

performGet ::
  Url ->
  RequestHeaders ->
  ResponseParser a ->
  Session a
performGet url requestHeaders =
  performRequest
    (assembleRawRequest "GET" url requestHeaders mempty)

performPost ::
  Url ->
  RequestHeaders ->
  ByteString ->
  ResponseParser a ->
  Session a
performPost url requestHeaders requestBody =
  performRequest
    (assembleRawRequest "POST" url requestHeaders requestBody)

performPut ::
  Url ->
  RequestHeaders ->
  ByteString ->
  ResponseParser a ->
  Session a
performPut url requestHeaders requestBody =
  performRequest
    (assembleRawRequest "PUT" url requestHeaders requestBody)

performRequest :: (Config -> Client.Request) -> ResponseParser a -> Session a
performRequest request (ResponseParser parseResponse) =
  Session $ \conf manager ->
    catch
      (Client.withResponse (request conf) manager parseResponse)
      (return . Left . normalizeRawException)

-- * Url

-------------------------

-- | Assemble a URL for a request.
url ::
  -- | HTTPS? HTTP otherwise.
  Bool ->
  -- | Host name.
  Host ->
  -- | Specific port if present. Default port for the protocol otherwise.
  Maybe Word16 ->
  -- | Path at the host.
  Path ->
  -- | Query params.
  [(Text, Text)] ->
  Url
url = Url

-- * HTTP-Client Assemblage

-------------------------

assemblePathString :: Path -> ByteString
assemblePathString (Path seq) =
  if Seq.null seq
    then "/"
    else
      Serialization.execute $
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
    step (k, v) next first bdr =
      next False newMason
      where
        newMason =
          if first
            then Serialization.asciiChar '?' <> param
            else bdr <> Serialization.asciiChar '&' <> param
          where
            param =
              if Text.null v
                then Serialization.percentEncodedQuerySegmentText k
                else
                  Serialization.percentEncodedQuerySegmentText k
                    <> Serialization.asciiChar '='
                    <> Serialization.percentEncodedQuerySegmentText v
    finalize _ bdr =
      Serialization.execute bdr

assembleRawHeaders :: RequestHeaders -> [(CI ByteString, ByteString)]
assembleRawHeaders (RequestHeaders seq) =
  fmap mapHeader (toList seq)
  where
    mapHeader (name, value) =
      (Ci.mk (Text.encodeUtf8 name), Text.encodeUtf8 value)

assembleRawRequest ::
  ByteString ->
  Url ->
  RequestHeaders ->
  ByteString ->
  Config ->
  Client.Request
assembleRawRequest method Url {..} requestHeaders body Config {..} =
  Client.defaultRequest
    { Client.host = coerce urlHost,
      Client.port = maybe (bool 80 443 urlSecure) fromIntegral urlPort,
      Client.secure = urlSecure,
      Client.requestHeaders = assembleRawHeaders requestHeaders,
      Client.path = assemblePathString urlPath,
      Client.queryString = assembleQueryString urlQuery,
      Client.requestBody = Client.RequestBodyBS body,
      Client.method = method,
      Client.redirectCount = configMaxRedirects,
      Client.responseTimeout = Client.responseTimeoutMicro (round (configTimeout * 1000000))
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
  lmap coerce . Extractor.atHashMapKey . Text.toCaseFold

-------------------------

deserializeBodyWithCereal :: Cereal.Get a -> ResponseParser a
deserializeBodyWithCereal get =
  ResponseParser $ go (Cereal.runGetPartial get) . Client.responseBody
  where
    go decode bodyReader = do
      chunk <- Client.brRead bodyReader
      case decode chunk of
        Cereal.Done res _ -> return $ Right res
        Cereal.Fail err _ -> return $ Left $ UnexpectedResponseErr $ to err
        Cereal.Partial decodeNext -> go decodeNext bodyReader

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

textHost :: Text -> Host
textHost =
  Host . Serialization.execute . Serialization.domain

-- * Path

-------------------------

textPath :: Text -> Path
textPath =
  Path . Seq.fromList . Text.split (== '/') . Text.dropAround (== '/')

-- * RequestHeaders

-------------------------

requestHeader :: Text -> Text -> RequestHeaders
requestHeader name value =
  RequestHeaders (pure (name, value))
