module TestTemplate (tests) where

import Data.Either (isLeft, isRight)
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Test.Tasty.QuickCheck qualified as QC

import Lux (Separator (..))
import Lux qualified as Lux
import Lux.Parser qualified as LP

templateMakingTests :: TestTree
templateMakingTests =
  testGroup
    "Making of templates"
    [ testGroup
        "Broken templates"
        [ testCase "Unclosed function call" $
            isLeft (Lux.makeTemplate "$foo")
              @? "Shouldn't accept non-closed function call"
        , testCase "Unknown function" $
            isLeft (Lux.makeTemplate "$foo;")
              @? "Shouldn't accept unknown function"
        , testCase "Several unknown functions" $
            case Lux.makeTemplate "$foo:bar:rev:baz:rev:bazzz:rev;" of
              Right _ -> error "impossible"
              Left es ->
                es
                  @?= [ Lux.Error "Unknown function: foo"
                      , Lux.Error "Unknown function: bar"
                      , Lux.Error "Unknown function: baz"
                      , Lux.Error "Unknown function: bazzz"
                      ]
        ]
    , testGroup
        "Correct templates"
        [ testCase "All known functions" $
            isRight
              ( Lux.makeTemplate $
                  mconcat
                    [ "$"
                    , T.intercalate ":" $ map (LP.unFName . fst) Lux.functions
                    , ";"
                    ]
              )
              @? "Should accept any registered function"
        ]
    ]

inputMakingTests :: TestTree
inputMakingTests =
  testGroup
    "Make input"
    [ testCase "Separating by ','" $
        Lux.iFields (Lux.makeInput (Lux.Separator ",") "a, b, c")
          @?= ["a", " b", " c"]
    ]

functionPropertyTests :: TestTree
functionPropertyTests =
  testGroup
    "Function properties"
    [ QC.testProperty "'rev' function" \s ->
        either (const False) id do
          template <- Lux.makeTemplate "$rev;"
          result <- Lux.apply template (Lux.makeInput (Separator ",") s)
          pure $ result == T.reverse s
    ]

indexingTests :: TestTree
indexingTests =
  testGroup
    "Indexing"
    [ testCase "Zero index should capture the whole input" $
        "$0" `appliedTo` "abcd" @?= Right "abcd"
    , testCase "Two fields should be swaped" $
        "$2,$1" `appliedTo` "a,b" @?= Right "b,a"
    , testCase "Functions should work well with indices" $
        "$2:rev:rev;,$1:rev;" `appliedTo` "abc,de" @?= Right "de,cba"
    , testCase "Index out of range" $
        "$20" `appliedTo` "a,b,c" @?= Left [Lux.Error "Index is out of range: 20"]
    , testCase "Negate index -1" $
        "$-1" `appliedTo` "a,b,c,d,e,f" @?= Right "f"
    , testCase "Negate index -3" $
        "$-3" `appliedTo` "a,b,c,d,e,f" @?= Right "d"
    , testCase "Negate index out of range" $
        "$-3" `appliedTo` "a,b" @?= Left [Lux.Error "Index is out of range: -3"]
    , testCase "To apply the function on the negate index" $
        "$-3:rev;" `appliedTo` "a,b,c,abcd,e,f" @?= Right "dcba"
    ]
  where
    appliedTo templateT inputString = do
      template <- Lux.makeTemplate templateT
      Lux.apply template $ Lux.makeInput (Separator ",") inputString

tests :: TestTree
tests =
  testGroup
    "Templating tests"
    [ templateMakingTests
    , inputMakingTests
    , functionPropertyTests
    , indexingTests
    ]
