module JevSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (AsyncException (ThreadKilled), throwIO, toException, try)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Jev
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Network.Wai hiding (Response, requestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Timeout qualified as Timeout
import Test.Hspec

data Team = Billing | Technical deriving (Eq, Show)

route :: Question (Choice Team)
route = choice "Route this" [Option Billing "billing" Nothing, Option Technical "technical" (Just "Bugs")]

choiceAnswer :: Value
choiceAnswer = object ["type" .= ("choice" :: Text), "choice" .= ("billing" :: Text), "confidence" .= (0.9 :: Double), "probabilities" .= object ["billing" .= (0.95 :: Double), "technical" .= (0.05 :: Double)]]

scoreAnswer :: Value
scoreAnswer = object ["type" .= ("score" :: Text), "score" .= (1.4 :: Double), "confidence" .= (0.6 :: Double), "probabilities" .= object ["1" .= (0.6 :: Double), "2" .= (0.4 :: Double)], "legend" .= object ["1" .= ("Today" :: Text), "2" .= ("Now" :: Text)]]

noulAnswer :: Double -> Value
noulAnswer p = object ["type" .= ("noul" :: Text), "noul" .= p]

envelope :: Value -> Value
envelope answers = object ["model" .= ("test-model" :: Text), "answers" .= answers, "usage" .= object ["input_tokens" .= (12 :: Int), "output_tokens" .= (3 :: Int)]]

serve :: Status -> LBS.ByteString -> Application
serve status body _ respond = respond (responseLBS status [(hContentType, "application/json")] body)

withServer :: Application -> (Client -> IO a) -> IO a
withServer application action = testWithApplication (pure application) $ \port ->
  withClient ((defaultConfig TypeSafe "test-key") {endpoint = Just ("http://127.0.0.1:" <> T.pack (show port) <> "/v1/systemone")}) action

answer :: Value -> Question a -> IO (Either JevError (Response a))
answer value question = withServer (serve status200 (encode (envelope value))) $ \client -> decide client "A prompt" question

isDecodeError :: Either JevError a -> Bool
isDecodeError (Left (DecodeError _)) = True
isDecodeError (Left (ResponseDecodeError _ _)) = True
isDecodeError _ = False

isValidationError :: Either JevError a -> Bool
isValidationError (Left (ValidationError _)) = True
isValidationError _ = False

spec :: Spec
spec = do
  describe "pure preparation and validation" $ do
    it "validates without credentials and identifies invalid composed questions" $ do
      validateQuestion TypeSafe route `shouldBe` Right ()
      validateQuestion TypeSafe ((,) <$> route <*> score "Bad rubric" ["Only"])
        `shouldBe` Left (ValidationError "q1 (score): Score requires 2 to 10 levels")
      validateQuestion TypeSafe (pure ()) `shouldSatisfy` isValidationError
    it "exposes the encoded request and decodes fixtures without a server" $ do
      case prepareRequest TypeSafe "test-model" (String "State") route of
        Left err -> expectationFailure (show err)
        Right prepared -> do
          case eitherDecode (requestBody prepared) of
            Right (Object body) -> do
              KM.lookup "state" body `shouldBe` Just (String "State")
              KM.lookup "model" body `shouldBe` Just (String "test-model")
            other -> expectationFailure (show (other :: Either String Value))
          fmap (\r -> r.answers.choice) (decodeResponse prepared (encode (envelope (object ["q0" .= choiceAnswer])))) `shouldBe` Right Billing
          decodeResponse prepared "not json" `shouldSatisfy` isDecodeError
    it "encodes OpenRouter settings and rejects incompatible settings locally" $ do
      let options = defaultRequestOptions {providerRouting = Just (object ["allow_fallbacks" .= False]), sessionId = Just "session", trace = Just (object ["trace_id" .= ("trace" :: Text)]), user = Just "user"}
      case prepareRequestWith OpenRouter options "model" (String "State") route of
        Left err -> expectationFailure (show err)
        Right prepared -> case eitherDecode (requestBody prepared) of
          Right (Object body) -> do
            KM.lookup "provider" body `shouldBe` options.providerRouting
            KM.lookup "session_id" body `shouldBe` Just (String "session")
            KM.lookup "trace" body `shouldBe` options.trace
            KM.lookup "user" body `shouldBe` Just (String "user")
          other -> expectationFailure (show (other :: Either String Value))
      let validateOptions provider settings = (() <$ prepareRequestWith provider settings "model" (String "State") route)
      validateOptions TypeSafe options `shouldSatisfy` isValidationError
      validateOptions OpenRouter (options {sessionId = Just (T.replicate 257 "x")}) `shouldSatisfy` isValidationError
      validateOptions OpenRouter (options {user = Just (T.replicate 257 "x")}) `shouldSatisfy` isValidationError
      validateOptions OpenRouter (options {providerRouting = Just Null}) `shouldSatisfy` isValidationError
      validateOptions OpenRouter (options {trace = Just (Bool True)}) `shouldSatisfy` isValidationError
    it "maps answer values without losing metadata or distribution entries" $ do
      let original = Response (Choice Billing 0.9 [(Billing, 0.95), (Technical, 0.05)]) "model" (Usage Nothing Nothing Nothing) (Just "id") Nothing
          mapped = fmap (fmap show) original
      mapped.answers `shouldBe` Choice "Billing" 0.9 [("Billing", 0.95), ("Technical", 0.05)]
      mapped.requestId `shouldBe` original.requestId
      mapped.usage `shouldBe` original.usage
  describe "typed results" $ do
    it "maps wire labels and probabilities to domain constructors" $ do
      result <- answer (object ["q0" .= choiceAnswer]) route
      fmap (\r -> r.answers) result `shouldBe` Right (Choice Billing 0.9 [(Billing, 0.95), (Technical, 0.05)])
    it "composes all primitives into one request independent of answer order" $ do
      result <- answer (object ["q2" .= noulAnswer 0.8, "q0" .= choiceAnswer, "q1" .= scoreAnswer]) ((,,) <$> route <*> score "Urgency" ["Routine", "Today", "Now"] <*> noul "Human?")
      fmap (\r -> let (c, s, n) = r.answers in (c.choice, s.score, s.probabilities, n.probability)) result
        `shouldBe` Right (Billing, 1.4, IM.fromList [(1, 0.6), (2, 0.4)], 0.8)
    it "supports nested composition and runtime collections" $ do
      result <- answer (object ["q0" .= noulAnswer 0.1, "q1" .= noulAnswer 0.9]) ((,) <$> pure "tag" <*> traverse noul ["One", "Two"])
      fmap (\r -> r.answers) result `shouldBe` Right ("tag" :: Text, [Noul 0.1, Noul 0.9])
    it "retains provider metadata and optional usage fields" $ do
      let body = object ["answers" .= object ["q0" .= noulAnswer 1], "model" .= ("typesafe/jev-1.13" :: Text), "id" .= ("request-1" :: Text), "provider" .= ("TypeSafe" :: Text), "usage" .= object ["cost" .= (0.001 :: Double)], "future" .= True]
      result <- withServer (serve status200 (encode body)) $ \client -> decide client "State" (noul "Yes?")
      fmap (\r -> (r.requestId, r.provider, r.usage)) result `shouldBe` Right (Just "request-1", Just "TypeSafe", Usage Nothing Nothing (Just 0.001))
  describe "documented structured content" $ do
    it "accepts TypeSafe null instructions and nullable Choice and Noul descriptions" $ do
      captured <- newIORef Nothing
      let levels = [object ["meaning" .= ("Routine" :: Text)], toJSON (["Urgent"] :: [Text])]
          scoreValue = object ["type" .= ("score" :: Text), "score" .= (1 :: Int), "confidence" .= (1 :: Int), "probabilities" .= object ["1" .= (1 :: Int)], "legend" .= object ["0" .= headLevel, "1" .= toJSON (["Urgent"] :: [Text])]]
          headLevel = object ["meaning" .= ("Routine" :: Text)]
          app request respond = do
            body <- strictRequestBody request
            writeIORef captured (decode body :: Maybe Value)
            serve status200 (encode (envelope (object ["q0" .= choiceAnswer, "q1" .= scoreValue, "q2" .= noulAnswer 0.9]))) request respond
          questions =
            (,,)
              <$> choiceJSON Null [JsonOption Billing "billing" (Just Null), JsonOption Technical "technical" Nothing]
              <*> scoreJSON Null levels
              <*> noulJSON Null (Just (NoulCriteria (String "Needs help") Null))
      result <- withServer app $ \client -> decideJSON client (toJSON (["Please help", "Duplicate payment"] :: [Text])) questions
      fmap (\r -> let (_, rating, _) = r.answers in rating.legend) result `shouldBe` Right (IM.fromList (zip [0 ..] levels))
      body <- readIORef captured
      case body of
        Just (Object o) -> case KM.lookup "questions" o of
          Just (Object qs) -> do
            KM.lookup "q0" qs `shouldBe` Just (object ["type" .= ("choice" :: Text), "instructions" .= Null, "criteria" .= object ["billing" .= Null, "technical" .= Null]])
            KM.lookup "q2" qs `shouldBe` Just (object ["type" .= ("noul" :: Text), "instructions" .= Null, "criteria" .= object ["true" .= ("Needs help" :: Text), "false" .= Null]])
          other -> expectationFailure (show other)
        other -> expectationFailure (show other)
    it "rejects null Score levels and invalid legend values" $ do
      withServer (serve status500 "must not be called") $ \client ->
        decide client "State" (scoreJSON (String "Urgency") [Null, String "High"]) >>= (`shouldSatisfy` isValidationError)
      let malformed = object ["type" .= ("score" :: Text), "score" .= (0 :: Int), "confidence" .= (1 :: Int), "probabilities" .= object ["0" .= (1 :: Int)], "legend" .= object ["0" .= True]]
      answer (object ["q0" .= malformed]) (score "Urgency" ["Low", "High"]) >>= (`shouldSatisfy` isDecodeError)
    it "rejects TypeSafe-only null fields before sending to OpenRouter" $ do
      calls <- newIORef (0 :: Int)
      let app request respond = modifyIORef' calls (+ 1) >> serve status500 "unexpected" request respond
      testWithApplication (pure app) $ \port ->
        withClient ((defaultConfig OpenRouter "key") {endpoint = Just ("http://127.0.0.1:" <> T.pack (show port))}) $ \client -> do
          decide client "State" (noulJSON Null Nothing) >>= (`shouldSatisfy` isValidationError)
          decide client "State" (noulJSON (String "Help?") (Just (NoulCriteria (String "Yes") Null))) >>= (`shouldSatisfy` isValidationError)
      readIORef calls `shouldReturn` 0
    it "accepts explicit null Choice descriptions through OpenRouter" $ do
      testWithApplication (pure (serve status200 (encode (envelope (object ["q0" .= choiceAnswer]))))) $ \port ->
        withClient ((defaultConfig OpenRouter "key") {endpoint = Just ("http://127.0.0.1:" <> T.pack (show port))}) $ \client -> do
          result <- decide client "State" (choiceJSON (String "Team?") [JsonOption Billing "billing" (Just Null), JsonOption Technical "technical" Nothing])
          fmap (\r -> r.answers.choice) result `shouldBe` Right Billing
    it "reads TypeSafe request IDs from headers and preserves gateway body IDs" $ do
      let bodyWithId = object ["model" .= ("test-model" :: Text), "usage" .= object [], "answers" .= object ["q0" .= noulAnswer 1], "id" .= ("gateway-id" :: Text)]
          app body _ respond = respond (responseLBS status200 [("x-typesafe-request-id", "direct-id")] (encode body))
      direct <- withServer (app (envelope (object ["q0" .= noulAnswer 1]))) $ \client -> decide client "State" (noul "Yes?")
      fmap (\r -> r.requestId) direct `shouldBe` Right (Just "direct-id")
      gateway <- withServer (app bodyWithId) $ \client -> decide client "State" (noul "Yes?")
      fmap (\r -> r.requestId) gateway `shouldBe` Right (Just "gateway-id")
  describe "validation" $ do
    it "rejects invalid questions before contacting the server" $ do
      calls <- newIORef (0 :: Int)
      let app request respond = modifyIORef' calls (+ 1) >> serve status500 "unexpected" request respond
      withServer app $ \client -> do
        decide client "State" (choice "Empty" ([] :: [Option Team])) >>= (`shouldSatisfy` isValidationError)
        decide client "State" (choice "Duplicate" [Option Billing "same" Nothing, Option Technical "same" Nothing]) >>= (`shouldSatisfy` isValidationError)
        decide client "State" (choice "Too many" [Option Billing (T.pack (show i)) Nothing | i <- [1 .. 256 :: Int]]) >>= (`shouldSatisfy` isValidationError)
        decide client "State" (score "Short" ["Only"]) >>= (`shouldSatisfy` isValidationError)
        decide client "State" (score "Long" (replicate 11 "Level")) >>= (`shouldSatisfy` isValidationError)
        decide client "State" (pure True) >>= (`shouldSatisfy` isValidationError)
        decideJSON client Null (noul "Yes?") >>= (`shouldSatisfy` isValidationError)
        decide client "State" (noulJSON (Bool True) Nothing) >>= (`shouldSatisfy` isValidationError)
      readIORef calls `shouldReturn` 0
  describe "response failures" $ do
    it "rejects missing answers and incorrect primitive tags" $ do
      answer (object []) route >>= (`shouldSatisfy` isDecodeError)
      answer (object ["q0" .= noulAnswer 0.5]) route >>= (`shouldSatisfy` isDecodeError)
    it "rejects unknown choices, missing probabilities, and invalid values" $ do
      let replace key value (Object o) = Object (KM.insert key value o)
          replace _ _ v = v
      answer (object ["q0" .= replace "choice" (String "unknown") choiceAnswer]) route >>= (`shouldSatisfy` isDecodeError)
      answer (object ["q0" .= replace "probabilities" (object []) choiceAnswer]) route >>= (`shouldSatisfy` isDecodeError)
      answer (object ["q0" .= noulAnswer 1.1]) (noul "Yes?") >>= (`shouldSatisfy` isDecodeError)
      answer (object ["q0" .= replace "score" (Number 3) scoreAnswer]) (score "Urgency" ["Low", "High"]) >>= (`shouldSatisfy` isDecodeError)
      answer (object ["q0" .= replace "probabilities" (object ["9" .= (1 :: Int)]) scoreAnswer]) (score "Urgency" ["Low", "Medium", "High"]) >>= (`shouldSatisfy` isDecodeError)
    it "reports malformed JSON and HTTP status bodies" $ do
      withServer (serve status200 "not json") (\client -> decide client "State" (noul "Yes?")) >>= (`shouldSatisfy` isDecodeError)
      mapM_
        ( \status -> do
            result <- withServer (serve status "provider error") (\client -> decide client "State" (noul "Yes?"))
            case result of
              Left (HttpError metadata body) -> do
                metadata.statusCode `shouldBe` statusCode status
                body `shouldBe` "provider error"
              other -> expectationFailure (show other)
        )
        [status401, status422, status429, mkStatus 529 "Overloaded"]
    it "rejects contradictory distributions while accepting rounding and sparse rubrics" $ do
      let replace key value (Object o) = Object (KM.insert key value o)
          replace _ _ v = v
          decodeFixture question value = do
            prepared <- prepareRequest TypeSafe "model" (String "State") question
            decodeResponse prepared (encode (envelope (object ["q0" .= value])))
      decodeFixture route (replace "probabilities" (object ["billing" .= (0 :: Int), "technical" .= (0 :: Int)]) choiceAnswer) `shouldSatisfy` isDecodeError
      decodeFixture route (replace "choice" (String "technical") choiceAnswer) `shouldSatisfy` isDecodeError
      fmap (\r -> r.answers.choice) (decodeFixture route (replace "probabilities" (object ["billing" .= (0.9501 :: Double), "technical" .= (0.05 :: Double)]) choiceAnswer)) `shouldBe` Right Billing
      let rubric = score "Urgency" ["Routine", "Today", "Now"]
      decodeFixture rubric (replace "score" (Number 0) scoreAnswer) `shouldSatisfy` isDecodeError
      decodeFixture rubric (replace "legend" (object []) scoreAnswer) `shouldSatisfy` isDecodeError
      fmap (\r -> r.answers.probabilities) (decodeFixture rubric scoreAnswer) `shouldBe` Right (IM.fromList [(1, 0.6), (2, 0.4)])
    it "accepts distributions rounded to two decimals and rejects clearly invalid ones" $ do
      let letters n = choice "Pick" [Option c (T.singleton c) Nothing | c <- take n ['a' ..]]
          rounded selected values = object ["type" .= ("choice" :: Text), "choice" .= selected, "confidence" .= (0.5 :: Double), "probabilities" .= object values]
          decodeFixture question value = do
            prepared <- prepareRequest OpenRouter "model" (String "State") question
            fmap (\r -> r.answers.choice) (decodeResponse prepared (encode (envelope (object ["q0" .= value]))))
      decodeFixture (letters 3) (rounded ("a" :: Text) ["a" .= (0.33 :: Double), "b" .= (0.33 :: Double), "c" .= (0.33 :: Double)]) `shouldBe` Right 'a'
      decodeFixture (letters 4) (rounded ("a" :: Text) ["a" .= (0.26 :: Double), "b" .= (0.25 :: Double), "c" .= (0.25 :: Double), "d" .= (0.25 :: Double)]) `shouldBe` Right 'a'
      decodeFixture (letters 3) (rounded ("a" :: Text) ["a" .= (0.36 :: Double), "b" .= (0.37 :: Double), "c" .= (0.27 :: Double)]) `shouldBe` Right 'a'
      decodeFixture route (rounded ("billing" :: Text) ["billing" .= (0.85 :: Double), "technical" .= (0.05 :: Double)]) `shouldSatisfy` isDecodeError
      decodeFixture route (rounded ("billing" :: Text) ["billing" .= (0.9 :: Double), "technical" .= (0.3 :: Double)]) `shouldSatisfy` isDecodeError
      decodeFixture (letters 3) (rounded ("a" :: Text) ["a" .= (0.34 :: Double), "b" .= (0.36 :: Double), "c" .= (0.3 :: Double)]) `shouldSatisfy` isDecodeError
  describe "error rendering" $ do
    it "includes status, request ID, and message but never headers" $ do
      let metadata = ResponseMetadata 503 [("Set-Cookie", "session=secret-cookie"), ("x-typesafe-request-id", "req-1")] (Just "req-1")
          httpText = renderJevError (HttpError metadata "upstream unavailable")
          decodeText = renderJevError (ResponseDecodeError metadata "Error in $.q0: Probabilities must sum to approximately one")
      httpText `shouldBe` "HttpError (status 503, request ID req-1): upstream unavailable"
      decodeText `shouldBe` "ResponseDecodeError (status 503, request ID req-1): Error in $.q0: Probabilities must sum to approximately one"
      mapM_ (\rendered -> rendered `shouldSatisfy` (not . T.isInfixOf "secret-cookie")) [httpText, decodeText]
      T.length (renderJevError (HttpError metadata (LBS.replicate 10000 120))) `shouldSatisfy` (< 600)
    it "retains retry headers and request IDs on HTTP and decoding failures" $ do
      let app status body _ respond = respond (responseLBS status [("Retry-After", "7"), ("x-typesafe-request-id", "header-id")] body)
      failed <- withServer (app status429 "limited") $ \client -> decide client "State" (noul "Yes?")
      case failed of
        Left (HttpError metadata body) -> do
          metadata.statusCode `shouldBe` 429
          metadata.requestId `shouldBe` Just "header-id"
          lookup "Retry-After" metadata.headers `shouldBe` Just "7"
          body `shouldBe` "limited"
        other -> expectationFailure (show other)
      malformed <- withServer (app status200 "not json") $ \client -> decide client "State" (noul "Yes?")
      case malformed of
        Left (ResponseDecodeError metadata _) -> metadata.requestId `shouldBe` Just "header-id"
        other -> expectationFailure (show other)
      gateway <- withServer (app status200 "{\"id\":\"body-id\"}") $ \client -> decide client "State" (noul "Yes?")
      case gateway of
        Left (ResponseDecodeError metadata _) -> metadata.requestId `shouldBe` Just "body-id"
        other -> expectationFailure (show other)
  describe "HTTP transport" $ do
    it "sends one authenticated request with structured inputs" $ do
      requests <- newIORef []
      let app request respond = do
            body <- strictRequestBody request
            modifyIORef' requests ((requestMethod request, rawPathInfo request, requestHeaders request, either (Left . T.pack) Right (eitherDecode body) :: Either Text Value) :)
            serve status200 (encode (envelope (object ["q0" .= choiceAnswer, "q1" .= noulAnswer 0.7]))) request respond
          instructions = object ["question" .= ("Route" :: Text)]
          question = (,) <$> choiceJSON instructions [JsonOption Billing "billing" (Just instructions), JsonOption Technical "technical" Nothing] <*> noulJSON (String "Human?") (Just (NoulCriteria instructions (String "No")))
      withServer app $ \client -> do
        result <- decideJSON client (object ["ticket" .= ("Broken" :: Text)]) question
        fmap (\r -> (fst r.answers).choice) result `shouldBe` Right Billing
      captured <- readIORef requests
      case captured of
        [(method, path, headers, Right (Object body))] -> do
          method `shouldBe` "POST"
          path `shouldBe` "/v1/systemone"
          lookup hAuthorization headers `shouldBe` Just "Bearer test-key"
          lookup hContentType headers `shouldBe` Just "application/json"
          KM.lookup "state" body `shouldBe` Just (object ["ticket" .= ("Broken" :: Text)])
          KM.lookup "model" body `shouldBe` Just (String "jev-latest")
          case KM.lookup "questions" body of
            Just (Object qs) -> do
              KM.size qs `shouldBe` 2
              KM.lookup "q0" qs `shouldBe` Just (object ["type" .= ("choice" :: Text), "instructions" .= instructions, "criteria" .= object ["billing" .= instructions, "technical" .= Null]])
              KM.lookup "q1" qs `shouldBe` Just (object ["type" .= ("noul" :: Text), "instructions" .= ("Human?" :: Text), "criteria" .= object ["true" .= instructions, "false" .= ("No" :: Text)]])
            other -> expectationFailure (show other)
        other -> expectationFailure (show other)
    it "supports OpenRouter configuration and a caller-owned manager" $ do
      let app request respond = do
            rawPathInfo request `shouldBe` "/api/alpha/decisions"
            body <- strictRequestBody request
            case either (Left . T.pack) Right (eitherDecode body) of
              Right (Object o) -> KM.lookup "model" o `shouldBe` Just (String "typesafe/jev-1.13")
              other -> expectationFailure (show (other :: Either Text Value))
            serve status200 (encode (envelope (object ["q0" .= noulAnswer 0.2]))) request respond
      testWithApplication (pure app) $ \port -> do
        manager <- newManager defaultManagerSettings
        let config = (defaultConfig OpenRouter "router-key") {endpoint = Just ("http://127.0.0.1:" <> T.pack (show port) <> "/api/alpha/decisions")}
            client = clientWithManager config manager
        closeClient client
        result <- decide client "State" (noul "Yes?")
        fmap (\r -> r.answers) result `shouldBe` Right (Noul 0.2)
    it "returns a timeout without exposing credentials" $ do
      let app request respond = threadDelay 200000 >> serve status200 "{}" request respond
      testWithApplication (pure app) $ \port ->
        withClient ((defaultConfig TypeSafe "secret") {endpoint = Just ("http://127.0.0.1:" <> T.pack (show port)), timeoutMicros = 10000}) $ \client -> do
          result <- decide client "State" (noul "Yes?")
          result `shouldBe` Left (TransportError DeadlineExceeded)

    it "enforces the deadline after response headers have arrived" $ do
      let app _ respond =
            respond
              ( responseStream
                  status200
                  []
                  ( \write flush -> do
                      write " "
                      flush
                      threadDelay 2000000
                      write "{}"
                  )
              )
      testWithApplication (pure app) $ \port ->
        withClient ((defaultConfig TypeSafe "secret") {endpoint = Just ("http://127.0.0.1:" <> T.pack (show port)), timeoutMicros = 50000}) $ \client -> do
          result <- Timeout.timeout 1000000 (decide client "State" (noul "Yes?"))
          result `shouldBe` Just (Left (TransportError DeadlineExceeded))

    it "categorizes manager failures without retaining request contents" $ do
      let cases = [(HTTP.ConnectionFailure (toException (userError "secret")), ConnectionFailed), (HTTP.InternalException (toException (userError "secret")), InternalTransportFailure), (HTTP.ResponseBodyTooShort 10 2, InvalidResponse)]
      mapM_
        ( \(failure, expected) -> do
            manager <- newManager (defaultManagerSettings {HTTP.managerModifyRequest = \request -> throwIO (HTTP.HttpExceptionRequest request failure)})
            decide (clientWithManager (defaultConfig TypeSafe "secret") manager) "private" (noul "Yes?") `shouldReturn` Left (TransportError expected)
        )
        cases

    it "selects the documented provider URLs without endpoint overrides" $ do
      let app = serve status200 (encode (envelope (object ["q0" .= noulAnswer 0.5])))
      testWithApplication (pure app) $ \port -> do
        forProviders port [(TypeSafe, "api.typesafe.ai", "/v1/systemone"), (OpenRouter, "openrouter.ai", "/api/alpha/decisions")]
    it "closes owned clients idempotently" $ do
      client <- newClient (defaultConfig TypeSafe "test-key")
      closeClient client
      closeClient client
      decide client "State" (noul "Yes?") `shouldReturn` Left (ValidationError "Client is closed")
    it "propagates asynchronous cancellation" $ do
      manager <- newManager (defaultManagerSettings {HTTP.managerModifyRequest = \_ -> throwIO ThreadKilled})
      let client = clientWithManager (defaultConfig TypeSafe "test-key") manager
      result <- try (decide client "State" (noul "Yes?")) :: IO (Either AsyncException (Either JevError (Response Noul)))
      result `shouldBe` Left ThreadKilled
  where
    forProviders port = mapM_ $ \(provider, host, path) -> do
      observed <- newIORef []
      manager <-
        newManager
          ( defaultManagerSettings
              { HTTP.managerModifyRequest = \request -> do
                  modifyIORef' observed ((HTTP.host request, HTTP.path request, HTTP.secure request) :)
                  pure request {HTTP.host = "127.0.0.1", HTTP.port = port, HTTP.secure = False, HTTP.proxy = Nothing}
              }
          )
      result <- decide (clientWithManager (defaultConfig provider "test-key") manager) "State" (noul "Yes?")
      fmap (\r -> r.answers) result `shouldBe` Right (Noul 0.5)
      requests <- readIORef observed
      take 1 (reverse requests) `shouldBe` [(host, path, True)]
