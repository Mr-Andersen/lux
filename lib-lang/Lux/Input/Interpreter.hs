module Lux.Input.Interpreter where

import Control.Applicative (asum)
import Control.Category ((>>>))
import Control.Monad.Except
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Writer
import Data.Bifoldable (Bifoldable (bifoldl, bifoldr))
import Data.Char (isLetter)
import Data.Functor ((<&>))
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (lookup, singleton)
import Data.Set qualified as Set (singleton)
import Data.Text (Text)
import Data.Text qualified as T (singleton)
import Data.Traversable (for)
import Lux.Common.Types
import Lux.Input.Match
import Lux.Input.Types
import Text.Megaparsec
import Text.Megaparsec.Braced
import Text.Megaparsec.Char (char, digitChar, letterChar, space1, spaceChar, string)
import Text.Megaparsec.Diagnostic
import Text.Megaparsec.NonEmpty (nonEmptyP)
import Text.Megaparsec.SepBy (SepBy ((:#)))
import Text.Megaparsec.SepBy qualified as SepBy (singleton, values)
import Text.Megaparsec.Spanned
import Text.PrettyPrint.HughesPJClass (pPrint)

data Context p = Context
  { patterns :: Map Identifier (p (Match Spanned))
  , source :: Text
  }

newtype BiList a b = BiList [(a, b)]

instance Bifoldable BiList where
  bifoldr f g z (BiList xs) = foldr (\(a, b) c -> f a (g b c)) z xs

interpret ::
  forall m p.
  ( MonadError DiagnosticMessage m
  , MonadWriter [DiagnosticMessage] m
  , MonadReader (Context p) m
  , MonadParsec DiagnosticMessage Text p
  , MonadWriter [DiagnosticMessage] p
  ) =>
  Spanned (Pattern Spanned) ->
  m (p (Match Spanned))
interpret spat = case spat.value of
  PIdent ident -> getParserByIdent (spanAs spat ident)
  PLiteral text ->
    pure $
      spanned (string text) <&> \raw -> Match {raw, data_ = MatchEmpty}
  PPatternPun (Braced {left, right, value}) ->
    interpret $
      spanAs spat $
        PBindRecord
          Braced
            { left
            , right
            , value =
                spanAs
                  value
                  BindRaw
                    { varName = spanAs value $ BindId $ FieldPath $ SepBy.singleton $ spanAs value $ NamedField $ value.value
                    , sep = spanAs value BindSep
                    , pat = spanAs value $ PIdent $ value.value
                    }
            }
  PModded Modded {pat, modifier} ->
    interpret pat <&> case modifier.value of
      ModMany ->
        spanned
          >>> many
          >>> fmap (maybe MatchEmpty MatchArray . nonEmpty)
          >>> dataToMatch
      ModSome ->
        spanned
          >>> nonEmptyP (pure ())
          >>> fmap MatchArray
          >>> dataToMatch
      ModOptional ->
        spanned
          >>> optional
          >>> fmap (maybe MatchEmpty (MatchArray . NE.singleton))
          >>> dataToMatch
      ModRepeat n ->
        spanned
          >>> replicateM n
          >>> fmap (maybe MatchEmpty MatchArray . nonEmpty)
          >>> dataToMatch
  PBindRecord (Braced {value}) -> case value.value of
    BindRaw {varName, pat} ->
      interpret pat >>= case varName.value of
        BindId (FieldPath fields) -> \parser -> do
          fields' <- for (SepBy.values fields) \field ->
            case field.value of
              NamedField ident -> pure ident
              IndexField _ ->
                withReaderM (.source) $
                  diagnosticThrow Nothing [spanAs field "Only named fields are supported on left hand side of binding"]
          pure $
            spanned parser <&> \data_ ->
              ( foldr
                  ( \(Identifier f) a ->
                      let data_' = MatchRecord $ Map.singleton f a
                       in spanAs a Match {raw = a.value.raw, data_ = data_'}
                  )
                  data_
                  fields'
              ).value
        BindDiscard -> spanned >>> fmap (const MatchEmpty) >>> dataToMatch >>> pure
  PPushArray Braced {value} ->
    interpret value <&> \parser -> do
      parsed <- spanned parser
      pure case parsed.value of
        Match {data_ = MatchArray _} -> parsed.value
        Match {raw, data_} -> Match {raw, data_ = MatchArray $ NE.singleton $ spanAs parsed $ Match {raw, data_}}
  PSequence (h :# t) -> bifoldl onSep onPat (interpretNested h) (BiList t)
    where
      interpretNested Spanned {value = PPushArray pat} = interpret pat.value
      interpretNested pat = interpret pat <&> \parser -> parser <&> \case
        Match {raw, data_ = MatchArray _} -> Match {raw, data_ = MatchEmpty}
        other -> other

      onSep :: m (p (Match Spanned)) -> Spanned SequenceSep -> m (p (Match Spanned))
      onSep getPAcc ssep =
        getPAcc >>= \pacc -> do
          source <- asks (.source)
          pure do
            acc <- pacc
            sepMb <- case ssep.value of
              SequenceSpace -> Nothing <$ space1
              SequenceConcat -> pure Nothing
              SequenceChar c -> spanned (char c) <&> Just . charToMatch
            case sepMb of
              Nothing -> pure acc
              Just sep -> flip runReaderT source do
                either fancy pure =<< runExceptT (acc `mergeMatch` sep)

      onPat :: m (p (Match Spanned)) -> Spanned (Pattern Spanned) -> m (p (Match Spanned))
      onPat getPAcc spat' =
        getPAcc >>= \pacc -> do
          source <- asks (.source)
          ppat <- interpretNested spat'
          pure $ flip runReaderT source do
            acc <- lift pacc
            new <- lift ppat
            either fancy pure =<< runExceptT (acc `mergeMatch` new)
  PChoice cs -> asum <$> traverse (fmap try . interpret) (SepBy.values cs)
  PBraced Braced {value} -> interpret value

fancy :: MonadParsec e s m => e -> m a
fancy = parseError . FancyError 0 . Set.singleton . ErrorCustom

getParserByIdent ::
  ( MonadError DiagnosticMessage m
  , MonadReader (Context p) m
  , MonadParsec e Text p
  ) =>
  Spanned Identifier ->
  m (p (Match Spanned))
getParserByIdent sident =
  case sident.value of
    "any" -> pure $ spanned anySingle <&> charToMatch
    "space" -> pure $ spanned spaceChar <&> charToMatch
    "alpha" -> pure $ spanned letterChar <&> charToMatch
    "digit" -> pure $ spanned digitChar <&> charToMatch
    "str" -> pure $ spanned (takeWhileP (Just "letter") isLetter) <&> strToMatch
    ident -> do
      parserMb <- asks $ Map.lookup ident . (.patterns)
      case parserMb of
        Nothing -> withReaderM (.source) $ diagnosticThrow Nothing [spanAs sident $ "Undefined parser " <> pPrint ident]
        Just ok -> pure ok

charToMatch :: Spanned Char -> Match Spanned
charToMatch schar = Match {raw = spanAs schar $ T.singleton $ schar.value, data_ = MatchEmpty}

strToMatch :: Spanned Text -> Match Spanned
strToMatch raw = Match {raw, data_ = MatchEmpty}

withReaderM :: MonadReader r m => (r -> r') -> ReaderT r' m a -> m a
withReaderM f a = asks f >>= runReaderT a
