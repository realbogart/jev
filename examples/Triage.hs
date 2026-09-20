module Main (main) where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Jev
import System.Environment (getEnv)

data Team = Billing | Technical deriving (Eq, Show)

data Triage = Triage
  {team :: Choice Team, urgency :: Score, human :: Noul}
  deriving (Eq, Show)

triage :: Question Triage
triage =
  Triage
    <$> choice
      "Which team should handle this?"
      [ Option Billing "billing" (Just "Payments and invoices"),
        Option Technical "technical" (Just "Bugs and outages")
      ]
    <*> score "How urgent is this?" ["Routine", "Handle today", "Handle immediately"]
    <*> noulWithCriteria
      "Does this need a human?"
      (NoulCriteria "A person must investigate or act" "A standard help article will resolve it")

main :: IO ()
main = do
  key <- Text.pack <$> getEnv "OPENROUTER_API_KEY"
  withClient (defaultConfig OpenRouter key) $ \client -> do
    result <- decide client "Checkout has failed all morning. Please help now." triage
    case result of
      Left err -> Text.putStrLn (Text.pack (show err))
      Right response -> do
        let answer = response.answers
        case answer.team.choice of
          Billing -> Text.putStrLn "Billing ticket"
          Technical -> Text.putStrLn "Technical ticket"
        Text.putStrLn ("Routing confidence: " <> Text.pack (show answer.team.confidence))
        Text.putStrLn ("Urgency (0–2): " <> Text.pack (show answer.urgency.score))
        if answer.human.probability >= 0.8
          then Text.putStrLn "Escalate to a person"
          else Text.putStrLn "Continue automated handling"
