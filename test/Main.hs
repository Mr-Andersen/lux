module Main where

import Test.Input qualified
import Test.Output qualified
import Test.Tasty (defaultMain, testGroup)
import TestParser qualified
import TestTemplate qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "Lux tests"
      [ TestParser.tests
      , TestTemplate.tests
      , Test.Input.tests
      , Test.Output.tests
      ]
