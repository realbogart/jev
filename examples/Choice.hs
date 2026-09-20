module Main (main) where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Jev
import System.Environment (getEnv)

data Team = Billing | Technical deriving (Eq, Show)

route :: Question (Choice Team)
route =
  choice
    "Which team should handle this?"
    [ Option Billing "billing" (Just "Payments and invoices"),
      Option Technical "technical" (Just "Bugs and outages")
    ]

main :: IO ()
main = do
  key <- Text.pack <$> getEnv "JEV_API_KEY"
  withClient (defaultConfig TypeSafe key) $ \client -> do
    result <- decide client "I was charged twice." route
    case result of
      Left err -> Text.putStrLn (Text.pack (show err))
      Right response -> case response.answers.choice of
        Billing -> Text.putStrLn "Send to billing"
        Technical -> Text.putStrLn "Send to technical support"
