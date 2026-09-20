-- | Pure question construction, validation, and fixture support.
module Jev.Question
  ( Question,
    choice,
    score,
    noul,
    noulWithCriteria,
    choiceJSON,
    scoreJSON,
    noulJSON,
    validateQuestion,
    PreparedRequest,
    prepareRequest,
    prepareRequestWith,
    requestBody,
    decodeResponse,
  )
where

import Data.Aeson (Value (String))
import Data.Text (Text)
import Jev.Internal.Protocol (PreparedRequest, Question, decodeResponse, prepareRequest, prepareRequestWith, requestBody, validateQuestion)
import Jev.Internal.Protocol qualified as Protocol
import Jev.Types

-- | Describe a choice with 1–255 options and unique wire labels.
-- Validation runs when prepared or sent; use 'validateQuestion' to check earlier.
choice :: Text -> [Option a] -> Question (Choice a)
choice instructions = choiceJSON (String instructions) . map (\(Option a label description) -> JsonOption a label (String <$> description))

-- | Rate on 2–10 ordered levels, starting at zero; returns a fractional expected score.
score :: Text -> [Text] -> Question Score
score instructions = scoreJSON (String instructions) . map String

-- | Ask a yes/no question; the answer is a probability, not a Boolean.
noul :: Text -> Question Noul
noul instructions = noulJSON (String instructions) Nothing

-- | Ask a yes/no question with explicit descriptions for both outcomes.
noulWithCriteria :: Text -> NoulCriteria Text -> Question Noul
noulWithCriteria instructions (NoulCriteria yes no) = noulJSON (String instructions) (Just (NoulCriteria (String yes) (String no)))

-- | Structured choice instructions and descriptions. Instructions accept strings,
-- objects, or arrays (also null for TypeSafe); descriptions may be null on either gateway.
choiceJSON :: Value -> [JsonOption a] -> Question (Choice a)
choiceJSON = Protocol.choiceQuestion

-- | Structured score instructions and 2–10 levels. Levels accept strings, objects,
-- or arrays, never null. Instructions may be null for TypeSafe only.
scoreJSON :: Value -> [Value] -> Question Score
scoreJSON = Protocol.scoreQuestion

-- | Structured yes/no instructions and optional outcome descriptions. Strings,
-- objects, and arrays are accepted; null is accepted for TypeSafe only.
noulJSON :: Value -> Maybe (NoulCriteria Value) -> Question Noul
noulJSON = Protocol.noulQuestion
