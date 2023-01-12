module Main where

import Test.Lux.Input.Interpreter qualified
import Test.Lux.Input.Parsers qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Lux tests"
      [ Test.Lux.Input.Parsers.tests
      , Test.Lux.Input.Interpreter.tests
      ]
