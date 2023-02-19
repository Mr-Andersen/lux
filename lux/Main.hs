module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (Identity(Identity))
import Control.Monad.Reader (runReader)
import Control.Monad.Writer (runWriterT)
import Data.Aeson (encode, ToJSON)
import Data.Aeson qualified as Aeson (Value(String, Array, Object))
import Data.Aeson.Types (ToJSON(toJSON))
import Data.ByteString.Lazy qualified as B (putStr)
import Data.Foldable (for_, Foldable (toList))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map (toList)
import Data.Text (Text)
import Data.Text qualified as T (unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy qualified as Text.Lazy (lines)
import Data.Text.Lazy.IO qualified as Text.Lazy (getContents)
import Data.Void (Void)
import Lux.Input.Interpreter (interpret, Context (Context, patterns, options), Options(Options, input))
import Lux.Input.Match (Match (Match, raw, data_), MatchData (..))
import Lux.Input.Parsers (pattern)
import Lux.Input.Types (Pattern)
import Lux.UnSpan (UnSpan(unSpan))
import Options.Applicative (Parser, strArgument, metavar, info, helper, fullDesc, progDesc, header, (<**>), ParserInfo, execParser, switch, long)
import System.IO (stderr, hPutStrLn)
import Text.Megaparsec (ShowErrorComponent, ParsecT, errorBundlePretty, runParserT, eof)
import Text.Megaparsec.Spanned (Spanned, spanned)
import Text.PrettyPrint.HughesPJClass (pPrint)
import GHC.Exts (fromList, fromString)

poptions :: Parser Options
poptions = Options <$> strArgument (metavar "INPUT") <*> switch (long "no-array-shortcut")

poptions' :: ParserInfo Options
poptions' = info (poptions <**> helper)
  (fullDesc
     <> progDesc "Parse each line of stdin with INPUT and print resulting data as JSON"
     <> header "hello - a test for optparse-applicative")

wrapParser :: (MonadFail m, ShowErrorComponent e) => ParsecT e Text m a -> String -> Text -> m a
wrapParser p n i = either (fail . errorBundlePretty) pure =<< runParserT (p <* eof) n i

pattern' :: MonadFail m => Text -> m (Spanned (Pattern Spanned))
pattern' = wrapParser (spanned (pattern @Void)) "pattern"

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

newtype Compact = Compact (Match Identity)

instance ToJSON Compact where
  toJSON (Compact Match{raw = Identity raw, data_ = MatchEmpty}) = Aeson.String raw
  toJSON (Compact Match{data_ = MatchArray a}) = Aeson.Array $ a & toList & map (\(Identity x) -> toJSON (Compact x)) & fromList
  toJSON (Compact Match{data_ = MatchRecord r}) = Aeson.Object $ r & Map.toList & map mapper & fromList where
    mapper (key, Identity val) = (fromString $ T.unpack key, toJSON (Compact val))

main :: IO ()
main = do
  options <- execParser poptions'
  pat <- pattern' options.input
  parser <-
    case interpret pat & runWriterT & runExceptT & flip runReader Context {patterns = mempty, options} of
      Left err -> let err' = show $ pPrint err
                   in ePutStrLn err' *> fail "Invalid input pattern"
      Right (parser, warns) -> do
        for_ warns $ ePutStrLn . ('\n' :) . show . pPrint
        pure parser
  xs <- Text.Lazy.getContents <&> Text.Lazy.lines
  for_ xs \x -> do
    (m, warns) <- wrapParser (runWriterT parser) "<input>" $ toStrict x
    for_ warns $ ePutStrLn . ('\n' :) . show . pPrint
    B.putStr $ encode (Compact (unSpan m)) <> "\n"
