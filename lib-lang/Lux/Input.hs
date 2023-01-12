module Lux.Input where

import Data.Text (Text)
import Data.Void (Void)

-- import Lux.Input.Interpreter
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import Control.Monad.Writer (WriterT (runWriterT))
import Data.Foldable (for_)
import Data.Function ((&))
import Lux.Input.Interpreter (Context (Context), interpret, patterns, source)
import Lux.Input.Parsers
import Lux.Input.Types (Pattern)
import Lux.UnSpan (unSpan)
import Text.Megaparsec
import Text.Megaparsec.Spanned
import Text.PrettyPrint.HughesPJClass (pPrint)

wrapParser :: (MonadFail m, ShowErrorComponent e) => ParsecT e Text m a -> String -> Text -> m a
wrapParser p n i = either (fail . errorBundlePretty) pure =<< runParserT (p <* eof) n i

pattern' :: MonadFail m => Text -> m (Spanned (Pattern Spanned))
pattern' = wrapParser (spanned (pattern @Void)) "pattern"

patternIO :: Text -> IO ()
patternIO s = pattern' s >>= print . unSpan . (.value)

interpretIO :: Text -> [Text] -> IO ()
interpretIO s xs = do
  pat <- pattern' s

  parser <-
    case interpret pat & runWriterT & runExceptT & flip runReader Context {patterns = mempty, source = s} of
      Left err -> fail $ show $ pPrint err
      Right (parser, warns) -> do
        for_ warns $ print . pPrint
        pure parser

  for_ xs \x -> do
    (m, warns) <- wrapParser (runWriterT parser) "<input>" x
    print $ unSpan m
    for_ warns $ print . pPrint
