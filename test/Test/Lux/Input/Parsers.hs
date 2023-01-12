module Test.Lux.Input.Parsers where

import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import Data.Text qualified as T (unpack)
import Data.Void (Void)
import Lux.Common.Types
import Lux.Input.Parsers (pattern)
import Lux.Input.Types
import Lux.Matches ((=~))
import Lux.UnSpan (UnSpan (unSpan))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Text.Megaparsec
import Text.Megaparsec.Braced
import Text.Megaparsec.SepBy (SepBy ((:#)))
import Text.Megaparsec.Spanned (Spanned)

parseSpanned :: MonadFail m => Text -> m (Pattern Spanned)
parseSpanned = either (fail . errorBundlePretty) pure . parse @Void (pattern <* eof) "tests"

(?=>) :: Text -> Pattern Identity -> TestTree
input ?=> p =
  testCase ("Parse " <> T.unpack input) do
    parsed <- parseSpanned input
    let errMsg =
          unlines
            [ "Input: " <> show input
            , "Expected: " <> show p
            , "Actual (unspanned): " <> show (unSpan parsed)
            ]
    assertBool errMsg $ parsed =~ p

tests :: TestTree
tests =
  testGroup
    "Lux.Input.Parsers tests"
    [ "ident"
        ?=> PIdent "ident"
    , "a,b,c"
        ?=> PSequence (Identity (PIdent "a") :# [(Identity SequenceConcat, Identity (PIdent "b")), (Identity SequenceConcat, Identity (PIdent "c"))])
    , "a|b|c"
        ?=> PChoice (Identity (PIdent "a") :# [(Identity ChoiceSep, Identity (PIdent "b")), (Identity ChoiceSep, Identity (PIdent "c"))])
    , "a,b|c"
        ?=> PChoice (Identity (PSequence (Identity (PIdent "a") :# [(Identity SequenceConcat, Identity (PIdent "b"))])) :# [(Identity ChoiceSep, Identity (PIdent "c"))])
    , "a|b,c"
        ?=> PChoice (Identity (PIdent "a") :# [(Identity ChoiceSep, Identity (PSequence (Identity (PIdent "b") :# [(Identity SequenceConcat, Identity (PIdent "c"))])))])
    , "{str}" ?=> PPatternPun (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity "str"})
    , "alpha+" ?=> PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome})
    , "[alpha+][digit+]" ?=> PSequence (Identity (PPushArray (Braced {left = Identity SquareBracketOpen, right = Identity SquareBracketClose, value = Identity (PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome}))})) :# [(Identity SequenceConcat, Identity (PPushArray (Braced {left = Identity SquareBracketOpen, right = Identity SquareBracketClose, value = Identity (PModded (Modded {pat = Identity (PIdent "digit"), modifier = Identity ModSome}))})))])
    , "{x:alpha}digit*{y:alpha}" ?=> PSequence (Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# []})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})) :# [(Identity SequenceConcat, Identity (PModded (Modded {pat = Identity (PIdent "digit"), modifier = Identity ModMany}))), (Identity SequenceConcat, Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "y") :# []})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})))])
    , "alpha+digit+" ?=> PSequence (Identity (PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome})) :# [(Identity SequenceConcat, Identity (PModded (Modded {pat = Identity (PIdent "digit"), modifier = Identity ModSome})))])
    , "{x:alpha+}{y:digit+}" ?=> PSequence (Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# []})), sep = Identity BindSep, pat = Identity (PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome}))})})) :# [(Identity SequenceConcat, Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "y") :# []})), sep = Identity BindSep, pat = Identity (PModded (Modded {pat = Identity (PIdent "digit"), modifier = Identity ModSome}))})})))])
    , "{x:{y:alpha}{z:alpha}}" ?=> PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# []})), sep = Identity BindSep, pat = Identity (PSequence (Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "y") :# []})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})) :# [(Identity SequenceConcat, Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "z") :# []})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})))]))})})
    , "{x:{y:alpha}+}" ?=> PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# []})), sep = Identity BindSep, pat = Identity (PModded (Modded {pat = Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "y") :# []})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})), modifier = Identity ModSome}))})})
    , "{alpha} {alpha}" ?=> PSequence (Identity (PPatternPun (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity "alpha"})) :# [(Identity SequenceSpace, Identity (PPatternPun (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity "alpha"})))])
    , "{x.y:alpha}{x.z:alpha}" ?=> PSequence (Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# [(Identity FieldPathSep, Identity (NamedField "y"))]})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})) :# [(Identity SequenceConcat, Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# [(Identity FieldPathSep, Identity (NamedField "z"))]})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})))])
    , "{x.y:alpha}{x.y:alpha}" ?=> PSequence (Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# [(Identity FieldPathSep, Identity (NamedField "y"))]})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})) :# [(Identity SequenceConcat, Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# [(Identity FieldPathSep, Identity (NamedField "y"))]})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})))])
    , "{x:{y:alpha}{z:alpha}}{x.y:alpha}" ?=> PSequence (Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# []})), sep = Identity BindSep, pat = Identity (PSequence (Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "y") :# []})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})) :# [(Identity SequenceConcat, Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "z") :# []})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})))]))})})) :# [(Identity SequenceConcat, Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# [(Identity FieldPathSep, Identity (NamedField "y"))]})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})))])
    , "{x:alpha+} {x:alpha+}" ?=> PSequence (Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# []})), sep = Identity BindSep, pat = Identity (PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome}))})})) :# [(Identity SequenceSpace, Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# []})), sep = Identity BindSep, pat = Identity (PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome}))})})))])
    , "[alpha+] [alpha+]" ?=> PSequence (Identity (PPushArray (Braced {left = Identity SquareBracketOpen, right = Identity SquareBracketClose, value = Identity (PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome}))})) :# [(Identity SequenceSpace, Identity (PPushArray (Braced {left = Identity SquareBracketOpen, right = Identity SquareBracketClose, value = Identity (PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome}))})))])
    , "[alpha+] alpha+" ?=> PSequence (Identity (PPushArray (Braced {left = Identity SquareBracketOpen, right = Identity SquareBracketClose, value = Identity (PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome}))})) :# [(Identity SequenceSpace, Identity (PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome})))])
    , "{x:alpha|digit}" ?=> PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# []})), sep = Identity BindSep, pat = Identity (PChoice (Identity (PIdent "alpha") :# [(Identity ChoiceSep, Identity (PIdent "digit"))]))})})
    , "{x:[alpha+][digit]}" ?=> PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# []})), sep = Identity BindSep, pat = Identity (PSequence (Identity (PPushArray (Braced {left = Identity SquareBracketOpen, right = Identity SquareBracketClose, value = Identity (PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome}))})) :# [(Identity SequenceConcat, Identity (PPushArray (Braced {left = Identity SquareBracketOpen, right = Identity SquareBracketClose, value = Identity (PIdent "digit")})))]))})})
    , "{x:[alpha+]\\,[digit]}" ?=> PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# []})), sep = Identity BindSep, pat = Identity (PSequence (Identity (PPushArray (Braced {left = Identity SquareBracketOpen, right = Identity SquareBracketClose, value = Identity (PModded (Modded {pat = Identity (PIdent "alpha"), modifier = Identity ModSome}))})) :# [(Identity (SequenceChar ','), Identity (PPushArray (Braced {left = Identity SquareBracketOpen, right = Identity SquareBracketClose, value = Identity (PIdent "digit")})))]))})})
    , "{x:[digit+]|{y:alpha}}" ?=> PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "x") :# []})), sep = Identity BindSep, pat = Identity (PChoice (Identity (PPushArray (Braced {left = Identity SquareBracketOpen, right = Identity SquareBracketClose, value = Identity (PModded (Modded {pat = Identity (PIdent "digit"), modifier = Identity ModSome}))})) :# [(Identity ChoiceSep, Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "y") :# []})), sep = Identity BindSep, pat = Identity (PIdent "alpha")})})))]))})})
    , "{str:{str:str}}" ?=> PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "str") :# []})), sep = Identity BindSep, pat = Identity (PBindRecord (Braced {left = Identity CurlyBraceOpen, right = Identity CurlyBraceClose, value = Identity (BindRaw {varName = Identity (BindId (FieldPath {val = Identity (NamedField "str") :# []})), sep = Identity BindSep, pat = Identity (PIdent "str")})}))})})
    ]
