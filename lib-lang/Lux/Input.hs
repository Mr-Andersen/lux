module Lux.Input where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Void (Void)
import Lux.Input.Interpret
import Lux.Input.Parsers
import Text.Megaparsec
import Text.Megaparsec.Spanned
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Lux.Input.Match (Match)

wrapParser :: MonadFail m => ParsecT Void Text m a -> String -> Text -> m a
wrapParser p n i = either (fail . errorBundlePretty) pure =<< runParserT (p <* eof) n i

testInterpreter :: Text -> Text -> IO (Match Spanned)
testInterpreter pattern input = do
  input' <- wrapParser (spanned patternChoice <* eof) "pattern" pattern
  intRes <- runExceptT $ wrapParser (runWriterT $ interpret TopLevel (spanValue input') <* eof) "input" input
  case intRes of
    Left msg -> fail . show $ pPrint msg
    Right (res, warns) -> do
      for_ warns (print . pPrint)
      pure res
