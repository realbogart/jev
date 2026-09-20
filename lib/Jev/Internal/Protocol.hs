module Jev.Internal.Protocol where

import Control.Monad (unless)
import Data.Aeson hiding (decode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (JSONPathElement (Key), Pair, Parser, parseEither)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Jev.Types

type role Question representational

-- | A pure description of independent questions. Applicative composition sends
-- all questions in one request; there is no Monad instance.
newtype Question a = Question
  {compile :: Provider -> Int -> Either JevError (Int, [(Key.Key, Value)], Object -> Parser a)}

instance Functor Question where
  fmap f (Question build) = Question $ \provider n -> do
    (next, fields, decode) <- build provider n
    pure (next, fields, fmap f . decode)

instance Applicative Question where
  pure a = Question $ \_ n -> Right (n, [], const (pure a))
  Question buildF <*> Question buildA = Question $ \provider n -> do
    (middle, fs, decodeF) <- buildF provider n
    (end, as, decodeA) <- buildA provider middle
    pure (end, fs <> as, \answers -> decodeF answers <*> decodeA answers)

validate :: Bool -> Text -> Either JevError ()
validate condition message = unless condition (Left (ValidationError message))

structured :: Value -> Bool
structured (String _) = True
structured (Object _) = True
structured (Array _) = True
structured _ = False

nullableContent :: Value -> Bool
nullableContent Null = True
nullableContent value = structured value

validDescription :: Provider -> Value -> Bool
validDescription TypeSafe = nullableContent
validDescription OpenRouter = structured

primitive :: Text -> Value -> Maybe Value -> (Provider -> Either JevError ()) -> (Object -> Parser a) -> Question a
primitive tag instructions criteria check decode = Question $ \provider n -> do
  let context = "q" <> T.pack (show n) <> " (" <> tag <> "): "
      contextualize (ValidationError message) = ValidationError (context <> message)
      contextualize err = err
  first contextualize $ do
    validate (validDescription provider instructions) "Instructions must be text, an object, or an array (null is supported by TypeSafe only)"
    check provider
  let key = Key.fromText ("q" <> T.pack (show n))
      body = object (["type" .= tag, "instructions" .= instructions] <> maybe [] (\c -> ["criteria" .= c]) criteria)
      parse answers = do
        value <- answers .: key
        (<?> Key key) $
          withObject
            "answer"
            ( \o -> do
                actual <- o .: "type"
                unless (actual == tag) (fail "Answer type does not match question")
                decode o
            )
            value
  pure (n + 1, [(key, body)], parse)

probabilityParser :: Double -> Parser Double
probabilityParser x
  | isNaN x || isInfinite x || x < 0 || x > 1 = fail "Probability must be finite and between zero and one"
  | otherwise = pure x

-- Allow small discrepancies from serialized floating-point distributions.
probabilityTolerance :: Double
probabilityTolerance = 1e-3

distribution :: [Double] -> Parser ()
distribution values = unless (abs (sum values - 1) <= probabilityTolerance) (fail "Probabilities must sum to approximately one")

choiceQuestion :: Value -> [JsonOption a] -> Question (Choice a)
choiceQuestion instructions options = primitive "choice" instructions (Just criteria) check $ \o -> do
  label <- o .: "choice"
  selected <- lookupOption label
  confidence <- o .: "confidence" >>= probabilityParser
  values <- o .: "probabilities" :: Parser (Map.Map Text Double)
  unless (Map.keysSet values == Map.keysSet mapping) (fail "Choice probabilities do not match options")
  probabilities <-
    traverse
      ( \(JsonOption a key _) -> do
          p <- maybe (fail "Missing option probability") probabilityParser (Map.lookup key values)
          pure (a, p)
      )
      options
  distribution (map snd probabilities)
  selectedProbability <- maybe (fail "Missing selected probability") pure (Map.lookup label values)
  unless (all (\p -> p <= selectedProbability + probabilityTolerance) (Map.elems values)) (fail "Selected choice is not a highest-probability option")
  pure (Choice selected confidence probabilities)
  where
    mapping = Map.fromList [(key, a) | JsonOption a key _ <- options]
    criteria = object [Key.fromText key .= description | JsonOption _ key description <- options]
    check _ = do
      validate (not (null options) && length options <= 255) "Choice requires 1 to 255 options"
      validate (Map.size mapping == length options) "Choice labels must be unique"
      validate (all (\(JsonOption _ _ d) -> maybe True nullableContent d) options) "Choice descriptions must be text, objects, arrays, null, or absent"
    lookupOption label = maybe (fail "Unknown choice label") pure (Map.lookup label mapping)

scoreQuestion :: Value -> [Value] -> Question Score
scoreQuestion instructions levels = primitive "score" instructions (Just (toJSON levels)) check $ \o -> do
  score <- o .: "score"
  unless (not (isNaN score || isInfinite score) && score >= 0 && score <= fromIntegral (length levels - 1)) (fail "Score outside rubric")
  confidence <- o .: "confidence" >>= probabilityParser
  probabilities <- o .: "probabilities" >>= indexed (\v -> parseJSON v >>= probabilityParser)
  unless (not (IM.null probabilities)) (fail "Score probabilities must not be empty")
  legend <- o .: "legend" >>= indexed (\value -> if structured value then pure value else fail "Invalid score legend description")
  distribution (IM.elems probabilities)
  unless (all (`IM.member` legend) (IM.keys probabilities)) (fail "Score legend is missing probability indices")
  let expected = sum [fromIntegral i * p | (i, p) <- IM.toList probabilities]
  unless (abs (score - expected) <= probabilityTolerance * fromIntegral (length levels)) (fail "Score does not match its probability-weighted rubric")
  pure (Score score confidence probabilities legend)
  where
    check _ = do
      validate (length levels >= 2 && length levels <= 10) "Score requires 2 to 10 levels"
      validate (all structured levels) "Score levels must be text, objects, or arrays"
    indexed :: (Value -> Parser b) -> Value -> Parser (IM.IntMap b)
    indexed parse = withObject "indexed rubric" $ \o ->
      IM.fromList
        <$> traverse
          ( \(key, value) -> do
              index <- maybe (fail "Unknown rubric index") pure (lookup (Key.toText key) [(T.pack (show i), i) | i <- [0 .. length levels - 1]])
              parsed <- parse value
              pure (index, parsed)
          )
          (KM.toList o)

noulQuestion :: Value -> Maybe (NoulCriteria Value) -> Question Noul
noulQuestion instructions criteria = primitive "noul" instructions encoded check $ \o -> Noul <$> (o .: "noul" >>= probabilityParser)
  where
    encoded = fmap (\(NoulCriteria yes no) -> object ["true" .= yes, "false" .= no]) criteria
    check provider = validate (maybe True (\(NoulCriteria yes no) -> validDescription provider yes && validDescription provider no) criteria) "Noul criteria must be text, objects, or arrays (null is supported by TypeSafe only)"

type role PreparedRequest representational

-- | Validated request bytes and the decoder tied to the original typed question.
-- Does not contain credentials. The body can contain sensitive application data.
data PreparedRequest a = PreparedRequest LBS.ByteString (LBS.ByteString -> Either JevError (Response a))

-- | Read the exact JSON body for inspection or a custom transport.
requestBody :: PreparedRequest a -> LBS.ByteString
requestBody (PreparedRequest body _) = body

-- | Decode a fixture or custom transport response using the original question mapping.
-- Does not check HTTP status or attach header metadata.
decodeResponse :: PreparedRequest a -> LBS.ByteString -> Either JevError (Response a)
decodeResponse (PreparedRequest _ decode) = decode

-- | Validate question structure without credentials or networking. Empty questions
-- are rejected; primitive errors identify the zero-based question ID.
validateQuestion :: Provider -> Question a -> Either JevError ()
validateQuestion provider (Question build) = do
  (_, fields, _) <- build provider 0
  validate (not (null fields)) "At least one question is required"

-- | Validate and encode a request without credentials or networking.
prepareRequest :: Provider -> Text -> Value -> Question a -> Either JevError (PreparedRequest a)
prepareRequest provider = prepareRequestWith provider defaultRequestOptions

-- | Like 'prepareRequest', with optional OpenRouter settings.
prepareRequestWith :: Provider -> RequestOptions -> Text -> Value -> Question a -> Either JevError (PreparedRequest a)
prepareRequestWith provider options model state (Question build) = do
  validate (not (T.null model)) "A model is required"
  extra <- requestOptions provider options
  validate (structured state) "State must be text, an object, or an array"
  (_, fields, parseAnswers) <- build provider 0
  validate (not (null fields)) "At least one question is required"
  let body = encode (object (["model" .= model, "state" .= state, "questions" .= Object (KM.fromList fields)] <> extra))
      decode bytes = do
        value <- either (Left . DecodeError . T.pack) Right (eitherDecode bytes)
        either
          (Left . DecodeError . T.pack)
          Right
          ( parseEither
              ( withObject "response" $ \o -> do
                  answers <- o .: "answers" >>= parseAnswers
                  resolved <- o .: "model"
                  usage <- o .: "usage" >>= withObject "usage" (\u -> Usage <$> u .:? "input_tokens" <*> u .:? "output_tokens" <*> u .:? "cost")
                  Response answers resolved usage <$> o .:? "id" <*> o .:? "provider"
              )
              value
          )
  pure (PreparedRequest body decode)

requestOptions :: Provider -> RequestOptions -> Either JevError [Pair]
requestOptions provider options = do
  validate (provider == OpenRouter || options == defaultRequestOptions) "Request options are supported by OpenRouter only"
  validate (maybe True isObject options.providerRouting) "Provider routing must be an object"
  validate (maybe True isObject options.trace) "Trace metadata must be an object"
  validate (maybe True ((<= 256) . T.length) options.sessionId) "Session ID must be at most 256 characters"
  validate (maybe True ((<= 256) . T.length) options.user) "User ID must be at most 256 characters"
  pure (field "provider" options.providerRouting <> field "session_id" options.sessionId <> field "trace" options.trace <> field "user" options.user)
  where
    isObject (Object _) = True
    isObject _ = False
    field :: (ToJSON a) => Key.Key -> Maybe a -> [Pair]
    field key = maybe [] (\value -> [key .= value])
