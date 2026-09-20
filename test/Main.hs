module Main where

import JevSpec qualified
import Test.Hspec

main :: IO ()
main = hspec JevSpec.spec
