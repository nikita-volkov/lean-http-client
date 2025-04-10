-- | A lightweight DSL providing for declarative definition of HTTP requests
-- and parsers.
--
-- The main premise of this library is that requests are by nature coupled
-- with expectations on the possible responses. You don't just issue a
-- request and expect to get anything in the response. There is always a
-- structure to the response that you expect. This assumption alone lets us
-- greatly simplify the API compared to lower-level libs like \"http-client\"
-- and \"wreq\" and makes the API much more type-safe.
--
-- It must be noted though that this library is in active development,
-- so some features may be missing and radical changes to the API may come.
--
-- Some inspiration for this library comes from [the \"http\" library of Elm]
-- (https://package.elm-lang.org/packages/elm/http/latest).
module LeanHttpClient
  ( -- * Execution
    Err (..),
    runSession,
    runSessionOnGlobalManager,
    runSessionOnTemporaryManager,

    -- * Session
    Session,

    -- ** Request issuing sessions
    get,
    post,
    put,

    -- ** Settings control
    overrideTimeout,
    overrideMaxRedirects,

    -- * Url
    Url,
    url,

    -- * Host
    Host,
    textHost,

    -- * Path
    Path,
    textPath,

    -- * Request headers
    RequestHeaders,
    requestHeader,

    -- * Response parsing
    ResponseParser,
    getStatus,
    expectOkStatus,
    extractHeaders,
    parseJsonBody,
    deserializeBodyWithCereal,

    -- * Response headers parsing
    ResponseHeaders,
    lookupInResponseHeaders,
  )
where

import qualified AesonValueParser as Avp
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
import qualified Network.HTTP.Client.TLS as ClientTls
import qualified Network.HTTP.Types as HttpTypes

-------------------------

-- | Error during session execution.
data Err
  = -- | Connection timed out.
    TimeoutErr
  | -- | Any other network-related problem.
    NetworkErr Text
  | -- | Response parsing.
    ResponseParsingErr Text
  deriving (Show, Eq)

-- | Sequence of actions performing HTTP communication using a shared
-- connection manager. Terminates on the first error.
newtype Session a
  = Session (Config -> Client.Manager -> IO (Either Err a))
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadError Err)
    via (ExceptT Err (ReaderT Config (ReaderT Client.Manager IO)))

-- | Parser of HTTP response. Use it to specify what you expect to receive
-- from the server, when executing a request.
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

-- | Host name.
newtype Host
  = Host ByteString

instance IsString Host where
  fromString =
    textHost . fromString

-- | Location at some host.
-- You can append them using the 'Monoid' instance.
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

-- | URL specifying the destination of a request.
data Url = Url
  { -- | HTTPS? HTTP otherwise.
    urlSecure :: !Bool,
    -- | Host name.
    urlHost :: !Host,
    -- | Specific port if present. Default port for the protocol otherwise.
    urlPort :: !(Maybe Int),
    -- | Path at the host.
    urlPath :: !Path,
    -- | Query params.
    urlQuery :: ![(Text, Text)]
  }

-- * Config

-------------------------

defaultConfig :: Config
defaultConfig =
  Config 30 10

-------------------------

-- | Execute a session on the provided manager,
-- with 30s timeout and maximum of 10 redirects for each request.
--
-- These settings can be overriden using 'overrideTimeout' and
-- 'overrideMaxRedirects'.
runSession :: Session a -> Client.Manager -> IO (Either Err a)
runSession (Session run) manager =
  run defaultConfig manager

-- | Execute session using 'runSession' on a global manager.
runSessionOnGlobalManager :: Session a -> IO (Either Err a)
runSessionOnGlobalManager session = do
  manager <- ClientTls.getGlobalManager
  runSession session manager

-- | Execute session using 'runSession' on a manager,
-- which only exists for the duration of that action.
runSessionOnTemporaryManager :: Session a -> IO (Either Err a)
runSessionOnTemporaryManager session = do
  manager <- ClientTls.newTlsManager
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

-- | Assemble a session which performs a @GET@ request.
get ::
  Url ->
  -- | Request headers.
  RequestHeaders ->
  -- | Response parser.
  ResponseParser a ->
  Session a
get url requestHeaders =
  performRequest
    (assembleRawRequest "GET" url requestHeaders mempty)

-- | Assemble a session which performs a @POST@ request.
post ::
  Url ->
  -- | Request headers.
  RequestHeaders ->
  -- | Request body.
  ByteString ->
  -- | Response parser.
  ResponseParser a ->
  Session a
post url requestHeaders requestBody =
  performRequest
    (assembleRawRequest "POST" url requestHeaders requestBody)

-- | Assemble a session which performs a @PUT@ request.
put ::
  Url ->
  -- | Request headers.
  RequestHeaders ->
  -- | Request body.
  ByteString ->
  -- | Response parser.
  ResponseParser a ->
  Session a
put url requestHeaders requestBody =
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
  Maybe Int ->
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
      Serialization.execute
        $ foldMap (mappend "/" . Serialization.percentEncodedPathSegmentText) seq

-- |
-- Create a query string with all those ampersands and percent-encoding
-- from textual key-value pairs.
--
-- If value in a pair is empty,
-- then no @=@ will be inserted.
assembleQueryString :: (Foldable f) => f (Text, Text) -> ByteString
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
      Client.port = fromMaybe (bool 80 443 urlSecure) urlPort,
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
  ResponseParser (pure . first ResponseParsingErr . Extractor.extract extractor)

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

getStatus :: ResponseParser (Int, Text)
getStatus =
  ResponseParser $ \response ->
    case Client.responseStatus response of
      HttpTypes.Status code msg ->
        case Text.decodeUtf8' msg of
          Left err -> return $ Left $ ResponseParsingErr $ "Status message: " <> fromString (show err)
          Right msg -> return $ Right $ (code, msg)

getBodyAsByteString :: ResponseParser ByteString
getBodyAsByteString =
  ResponseParser $ \response ->
    fmap (Right . mconcat) $ Client.brConsume $ Client.responseBody response

expectOkStatus :: ResponseParser ()
expectOkStatus = do
  (code, msg) <- getStatus
  if code >= 300 || code < 200
    then do
      body <- getBodyAsByteString
      throwError $ ResponseParsingErr $ from @TextBuilder $ renderErr code msg body
    else return ()
  where
    renderErr code msg body =
      case Text.decodeUtf8' body of
        Right body
          | not (Text.null body) ->
              "Bad status: " <> (from . show) code <> ": " <> to msg <> ": " <> to body
        _ ->
          "Bad status: " <> (from . show) code <> ": " <> to msg

deserializeBodyWithCereal :: Cereal.Get a -> ResponseParser a
deserializeBodyWithCereal get =
  ResponseParser $ go (Cereal.runGetPartial get) . Client.responseBody
  where
    go decode bodyReader = do
      chunk <- Client.brRead bodyReader
      case decode chunk of
        Cereal.Done res rem ->
          if ByteString.null rem
            then return $ Right res
            else return $ Left $ ResponseParsingErr "Too much input"
        Cereal.Fail err _ -> return $ Left $ ResponseParsingErr $ from err
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
               in Left $ ResponseParsingErr $ msg
            BsAttoparsec.Partial _ ->
              Left (ResponseParsingErr "Response data interrupted")
        )

extractAesonValue :: Extractor Aeson.Value a -> ResponseParser a
extractAesonValue extractor =
  do
    aesonValue <- parseResponseBodyBytes Data.Aeson.Parser.json
    ResponseParser $ const $ pure $ first ResponseParsingErr $ Extractor.extract extractor aesonValue

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
