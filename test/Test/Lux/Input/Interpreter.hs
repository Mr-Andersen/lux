module Test.Lux.Input.Interpreter where

import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.Writer (WriterT (runWriterT))
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.Functor.Identity (Identity (Identity))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as T (unpack)
import Data.Traversable (for)
import Data.Void (Void)
import GHC.Exts (fromList)
import Lux.Input.Interpreter (Context (Context), interpret, patterns, options, Options(Options, input, noArrayShortcut))
import Lux.Input.Match (Match (Match, data_, raw), MatchData (MatchArray, MatchEmpty, MatchRecord))
import Lux.Input.Parsers (pattern)
import Lux.Input.Types (Pattern)
import Lux.UnSpan (UnSpan (unSpan))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Text.Megaparsec
import Text.Megaparsec.Spanned (Spanned, spanned)
import Text.PrettyPrint.HughesPJClass (pPrint)

wrapParser :: (MonadFail m, ShowErrorComponent e) => ParsecT e Text m a -> String -> Text -> m a
wrapParser p n i = either (fail . errorBundlePretty) pure =<< runParserT (p <* eof) n i

pattern' :: MonadFail m => Text -> m (Spanned (Pattern Spanned))
pattern' = wrapParser (spanned (pattern @Void <* eof)) "pattern"

interpretId :: MonadFail m => Text -> [Text] -> m [Match Identity]
interpretId pat xs = do
  pat' <- pattern' pat

  parser <-
    case interpret pat' & runWriterT & runExceptT & flip runReader Context {patterns = mempty, options = Options{input = pat, noArrayShortcut = False}} of
      Left err -> fail $ ('\n' :) $ show $ pPrint err
      Right (parser, []) -> pure parser
      Right (_, warns) -> fail $ ('\n' :) $ show $ foldMap pPrint warns

  for xs \x -> do
    (m, warns) <- wrapParser (runWriterT parser) "<input>" x
    unless (null warns) do
      fail $ ('\n' :) $ show $ foldMap pPrint warns
    pure $ unSpan m

checkInterpret :: Text -> [Text] -> [Match Identity] -> TestTree
checkInterpret pat sources expecteds =
  testCase ("Interpret " <> T.unpack pat) do
    actuals <- interpretId pat sources
    let zipper src = assertEqual ("Testing interpretation of \"" <> T.unpack src <> "\" failed")
    sequenceA_ $ zipWith3 zipper sources expecteds actuals

tests :: TestTree
tests =
  testGroup
    "Lux.Input.Interpreter tests"
    [ checkInterpret "str" ["abc"] [Match {raw = Identity "abc", data_ = MatchEmpty}]
    , checkInterpret "[alpha][digit]" ["a1"] [Match {raw = Identity "a1", data_ = MatchArray (Identity Match {raw = "a", data_ = MatchEmpty} :| [Identity Match {raw = "1", data_ = MatchEmpty}])}]
    , checkInterpret "alpha+" ["abc"] [Match {raw = Identity "abc", data_ = MatchArray (Identity (Match {raw = Identity "a", data_ = MatchEmpty}) :| [Identity (Match {raw = Identity "b", data_ = MatchEmpty}), Identity (Match {raw = Identity "c", data_ = MatchEmpty})])}]
    , checkInterpret "{str}" ["abc"] [Match {raw = Identity "abc", data_ = MatchRecord (fromList [("str", Identity (Match {raw = Identity "abc", data_ = MatchEmpty}))])}]
    , checkInterpret "[alpha+][digit+]" ["abc123"] [Match {raw = Identity "abc123", data_ = MatchArray (Identity (Match {raw = Identity "a", data_ = MatchEmpty}) :| [Identity (Match {raw = Identity "b", data_ = MatchEmpty}), Identity (Match {raw = Identity "c", data_ = MatchEmpty}), Identity (Match {raw = Identity "1", data_ = MatchEmpty}), Identity (Match {raw = Identity "2", data_ = MatchEmpty}), Identity (Match {raw = Identity "3", data_ = MatchEmpty})])}]
    , checkInterpret "{x:alpha}digit*{y:alpha}" ["a12b"] [Match {raw = Identity "a12b", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "a", data_ = MatchEmpty})),("y",Identity (Match {raw = Identity "b", data_ = MatchEmpty}))])}]
    , checkInterpret "alpha+digit+" ["abc123"] [Match {raw = Identity "abc123", data_ = MatchEmpty}]
    , checkInterpret "{x:alpha+}{y:digit+}" ["abc123"] [Match {raw = Identity "abc123", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "abc", data_ = MatchArray (Identity (Match {raw = Identity "a", data_ = MatchEmpty}) :| [Identity (Match {raw = Identity "b", data_ = MatchEmpty}),Identity (Match {raw = Identity "c", data_ = MatchEmpty})])})),("y",Identity (Match {raw = Identity "123", data_ = MatchArray (Identity (Match {raw = Identity "1", data_ = MatchEmpty}) :| [Identity (Match {raw = Identity "2", data_ = MatchEmpty}),Identity (Match {raw = Identity "3", data_ = MatchEmpty})])}))])}]
    , checkInterpret "{x:{y:alpha}{z:alpha}}" ["ab"] [Match {raw = Identity "ab", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "ab", data_ = MatchRecord (fromList [("y",Identity (Match {raw = Identity "a", data_ = MatchEmpty})),("z",Identity (Match {raw = Identity "b", data_ = MatchEmpty}))])}))])}]
    , checkInterpret "{x:{y:alpha}+}" ["abc"] [Match {raw = Identity "abc", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "abc", data_ = MatchArray (Identity (Match {raw = Identity "a", data_ = MatchRecord (fromList [("y",Identity (Match {raw = Identity "a", data_ = MatchEmpty}))])}) :| [Identity (Match {raw = Identity "b", data_ = MatchRecord (fromList [("y",Identity (Match {raw = Identity "b", data_ = MatchEmpty}))])}),Identity (Match {raw = Identity "c", data_ = MatchRecord (fromList [("y",Identity (Match {raw = Identity "c", data_ = MatchEmpty}))])})])}))])}]
    -- , checkInterpret "{alpha} {alpha}" ["a b"] [Match "" MatchEmpty]
    , checkInterpret "{x.y:alpha}{x.z:alpha}" ["ab"] [Match {raw = Identity "ab", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "ab", data_ = MatchRecord (fromList [("y",Identity (Match {raw = Identity "a", data_ = MatchEmpty})),("z",Identity (Match {raw = Identity "b", data_ = MatchEmpty}))])}))])}]
    , checkInterpret "{x.y:alpha}{x.y:alpha}" ["ab"] [Match {raw = Identity "ab", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "ab", data_ = MatchRecord (fromList [("y",Identity (Match {raw = Identity "ab", data_ = MatchEmpty}))])}))])}]
    , checkInterpret "{x:alpha}{y:alpha}{x:alpha}" ["abc"] [Match {raw = Identity "abc", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "ac", data_ = MatchEmpty})),("y",Identity (Match {raw = Identity "b", data_ = MatchEmpty}))])}]
    , checkInterpret "{x:{y:alpha}{z:alpha}}{x.y:alpha}" ["abc"] [Match {raw = Identity "abc", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "abc", data_ = MatchRecord (fromList [("y",Identity (Match {raw = Identity "ac", data_ = MatchEmpty})),("z",Identity (Match {raw = Identity "b", data_ = MatchEmpty}))])}))])}]
    , checkInterpret "{x:alpha+} {x:alpha+}" ["a b"] [Match {raw = Identity "ab", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "ab", data_ = MatchArray (Identity (Match {raw = Identity "a", data_ = MatchEmpty}) :| [Identity (Match {raw = Identity "b", data_ = MatchEmpty})])}))])}]
    , checkInterpret "[alpha+] [alpha+]" ["a b"] [Match {raw = Identity "ab", data_ = MatchArray (Identity (Match {raw = Identity "a", data_ = MatchEmpty}) :| [Identity (Match {raw = Identity "b", data_ = MatchEmpty})])}]
    , checkInterpret "[alpha+] alpha+" ["a b"] [Match {raw = Identity "ab", data_ = MatchArray (Identity (Match {raw = Identity "a", data_ = MatchEmpty}) :| [])}]
    , checkInterpret "{x:alpha|digit}"
        ["a", "1"]
        [ Match {raw = Identity "a", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "a", data_ = MatchEmpty}))])}
        , Match {raw = Identity "1", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "1", data_ = MatchEmpty}))])}]
    , checkInterpret "{x:[alpha+][digit]}" ["ab1"] [Match {raw = Identity "ab1", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "ab1", data_ = MatchArray (Identity (Match {raw = Identity "a", data_ = MatchEmpty}) :| [Identity (Match {raw = Identity "b", data_ = MatchEmpty}),Identity (Match {raw = Identity "1", data_ = MatchEmpty})])}))])}]
    , checkInterpret "{x:[alpha+]\\,[digit]}" ["ab,1"] [Match {raw = Identity "ab,1", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "ab,1", data_ = MatchArray (Identity (Match {raw = Identity "a", data_ = MatchEmpty}) :| [Identity (Match {raw = Identity "b", data_ = MatchEmpty}),Identity (Match {raw = Identity "1", data_ = MatchEmpty})])}))])}]
    , checkInterpret "{x:[alpha+][','][digit]}" ["ab,1"] [Match {raw = Identity "ab,1", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "ab,1", data_ = MatchArray (Identity (Match {raw = Identity "a", data_ = MatchEmpty}) :| [Identity (Match {raw = Identity "b", data_ = MatchEmpty}),Identity (Match {raw = Identity ",", data_ = MatchEmpty}),Identity (Match {raw = Identity "1", data_ = MatchEmpty})])}))])}]
    , checkInterpret "{x:[digit+]|{y:alpha}}" ["123", "a"] [Match {raw = Identity "123", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "123", data_ = MatchArray (Identity (Match {raw = Identity "1", data_ = MatchEmpty}) :| [Identity (Match {raw = Identity "2", data_ = MatchEmpty}),Identity (Match {raw = Identity "3", data_ = MatchEmpty})])}))])}, Match {raw = Identity "a", data_ = MatchRecord (fromList [("x",Identity (Match {raw = Identity "a", data_ = MatchRecord (fromList [("y",Identity (Match {raw = Identity "a", data_ = MatchEmpty}))])}))])}]
    , checkInterpret "{str:{str:str}}" ["abc"] [Match {raw = Identity "abc", data_ = MatchRecord (fromList [("str",Identity (Match {raw = Identity "abc", data_ = MatchRecord (fromList [("str",Identity (Match {raw = Identity "abc", data_ = MatchEmpty}))])}))])}]
    ]
