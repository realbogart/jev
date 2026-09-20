-- | Reusable HTTPS clients with explicit credentials, deadlines, and no automatic
-- application retries. A borrowed manager retains its own transport retry policy.
module Jev.Client (Client, newClient, clientWithManager, closeClient, withClient, decide, decideJSON, decideWith, decideJSONWith) where

import Control.Applicative ((<|>))
import Control.Exception (bracket, try)
import Data.Aeson (Value (String), decode, (.:?))
import Data.Aeson.Types (parseMaybe, withObject)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Jev.Internal.Protocol (Question, decodeResponse, prepareRequestWith, requestBody)
import Jev.Types
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status qualified as Status
import System.Timeout (timeout)

-- | A reusable connection pool and immutable configuration. Share across requests.
data Client = Client Config (IO (Maybe HTTP.Manager)) (IO ())

-- | Create a TLS client with transport retries disabled. Prefer 'withClient' for
-- scoped use, or call 'closeClient' when the client is no longer needed.
newClient :: Config -> IO Client
newClient config = do
  manager <- HTTP.newManager (tlsManagerSettings {HTTP.managerRetryableException = const False})
  reference <- newIORef (Just manager)
  pure (Client config (readIORef reference) (writeIORef reference Nothing))

-- | Borrow a manager without taking ownership of its lifecycle or settings.
-- Closing this client is a no-op, including subsequent requests through it.
clientWithManager :: Config -> HTTP.Manager -> Client
clientWithManager config manager = Client config (pure (Just manager)) (pure ())

-- | Release an owned manager reference and reject subsequent requests. Idempotent;
-- in-flight requests may finish. Connections are reclaimed by the HTTP library
-- when the manager becomes unreachable, not synchronously by this function.
closeClient :: Client -> IO ()
closeClient (Client _ _ release) = release

-- | Bracket an owned client's lifecycle, also releasing it on exceptions.
withClient :: Config -> (Client -> IO a) -> IO a
withClient config = bracket (newClient config) closeClient

-- | Evaluate all composed questions in one request against a shared text state.
decide :: Client -> Text -> Question a -> IO (Either JevError (Response a))
decide client = decideWith client defaultRequestOptions

-- | Evaluate against a string, object, or array state. Null is rejected.
decideJSON :: Client -> Value -> Question a -> IO (Either JevError (Response a))
decideJSON client = decideJSONWith client defaultRequestOptions

-- | Like 'decide', with per-request OpenRouter routing and observability settings.
decideWith :: Client -> RequestOptions -> Text -> Question a -> IO (Either JevError (Response a))
decideWith client options state = decideJSONWith client options (String state)

-- | Like 'decideJSON', with per-request settings. The HTTP deadline includes
-- connection setup, request transmission, and complete response body consumption.
-- Pure validation and decoding are outside the deadline. Cancellation propagates.
decideJSONWith :: Client -> RequestOptions -> Value -> Question a -> IO (Either JevError (Response a))
decideJSONWith (Client config getManager _) options state question
  | T.null config.apiKey = pure (Left (ValidationError "An API key is required"))
  | config.timeoutMicros <= 0 = pure (Left (ValidationError "Timeout must be positive"))
  | otherwise = case prepareRequestWith config.provider options config.model state question of
      Left err -> pure (Left err)
      Right prepared -> do
        available <- getManager
        case available of
          Nothing -> pure (Left (ValidationError "Client is closed"))
          Just manager -> send manager prepared
  where
    send manager prepared = do
      result <- timeout config.timeoutMicros $ try $ do
        initial <- HTTP.parseRequest (T.unpack endpoint)
        let request =
              initial
                { HTTP.method = "POST",
                  HTTP.requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 config.apiKey), ("Content-Type", "application/json")],
                  HTTP.requestBody = HTTP.RequestBodyLBS (requestBody prepared),
                  HTTP.responseTimeout = HTTP.responseTimeoutNone,
                  HTTP.redirectCount = 0,
                  HTTP.checkResponse = \_ _ -> pure ()
                }
        HTTP.httpLbs request manager
      pure $ case result of
        Nothing -> Left (TransportError DeadlineExceeded)
        Just (Left (err :: HTTP.HttpException)) -> Left (TransportError (transportFailure err))
        Just (Right response) ->
          let status = Status.statusCode (HTTP.responseStatus response)
              body = HTTP.responseBody response
              headers = HTTP.responseHeaders response
              headerId = lookup "x-typesafe-request-id" headers >>= either (const Nothing) Just . TE.decodeUtf8'
              bodyId = decode body >>= parseMaybe (withObject "response" (.:? "id")) >>= id
              metadata = ResponseMetadata status headers (bodyId <|> headerId)
           in if status >= 200 && status < 300
                then case decodeResponse prepared body of
                  Left (DecodeError message) -> Left (ResponseDecodeError metadata message)
                  Left err -> Left err
                  Right (Response answers model usage bodyRequestId provider) -> Right (Response answers model usage (bodyRequestId <|> headerId) provider)
                else Left (HttpError metadata body)
    endpoint = case config.endpoint of
      Just url -> url
      Nothing -> case config.provider of
        TypeSafe -> "https://api.typesafe.ai/v1/systemone"
        OpenRouter -> "https://openrouter.ai/api/alpha/decisions"

transportFailure :: HTTP.HttpException -> TransportFailure
transportFailure (HTTP.InvalidUrlException _ _) = InvalidEndpoint
transportFailure (HTTP.HttpExceptionRequest _ content) = case content of
  HTTP.ResponseTimeout -> ResponseTimedOut
  HTTP.ConnectionTimeout -> ConnectionTimedOut
  HTTP.ConnectionFailure _ -> ConnectionFailed
  HTTP.ConnectionClosed -> ConnectionClosed
  HTTP.NoResponseDataReceived -> ConnectionClosed
  HTTP.InvalidStatusLine _ -> InvalidResponse
  HTTP.InvalidHeader _ -> InvalidResponse
  HTTP.ResponseBodyTooShort _ _ -> InvalidResponse
  HTTP.InvalidChunkHeaders -> InvalidResponse
  HTTP.IncompleteHeaders -> InvalidResponse
  HTTP.TlsNotSupported -> TlsNotSupported
  HTTP.InternalException _ -> InternalTransportFailure
  _ -> OtherTransportFailure
