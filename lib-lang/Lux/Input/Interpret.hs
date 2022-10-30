module Lux.Input.Interpret where

import Control.Applicative (asum)
import Control.Monad.Except
import Control.Monad.Writer
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map (singleton)
import Data.Text (Text)
import Data.Traversable (for)
import Lux.Common.Types
import Lux.Input.Match
import Lux.Input.Types
import Text.Megaparsec
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Diagnostic
import Text.Megaparsec.NonEmpty (nonEmptyP)
import Text.Megaparsec.SepBy (SepBy ((:#)))
import Text.Megaparsec.SepBy qualified as SepBy (values)
import Text.Megaparsec.Spanned

data InterpretTarget = TopLevel | Embedded

interpret ::
  ( MonadError DiagnosticMessage m
  , MonadWriter [DiagnosticMessage] m
  , MonadParsec e Text m
  ) =>
  InterpretTarget ->
  PatternChoice Spanned ->
  m (Match Spanned)
interpret target pat = dataToMatch $ interpretChoice target pat

interpretChoice ::
  (MonadError DiagnosticMessage m, MonadWriter [DiagnosticMessage] m, MonadParsec e Text m) =>
  InterpretTarget ->
  PatternChoice Spanned ->
  m (MatchData Spanned)
interpretChoice target PatternChoice {val} = asum (try . interpretSeq target . spanValue <$> SepBy.values val)

interpretSeq ::
  (MonadError DiagnosticMessage m, MonadWriter [DiagnosticMessage] m, MonadParsec e Text m) =>
  InterpretTarget ->
  PatternSequence Spanned ->
  m (MatchData Spanned)
interpretSeq target (PatternSequence (h :# t)) = do
  h' <- interpretModded $ spanValue h
  t' <- for t \(sep, x) -> interpretSep (spanValue sep) *> interpretModded (spanValue x)
  foldM mergeData MatchEmpty $ filterValues $ h' : t'
  where
    filterValues = case target of
      Embedded -> id
      TopLevel -> filter \case
        MatchArray _ -> False
        _ -> True

interpretSep :: (MonadParsec e Text m) => SequenceSep -> m ()
interpretSep SepComma = pure ()
interpretSep SepSpace = space1

interpretModded ::
  (MonadError DiagnosticMessage m, MonadWriter [DiagnosticMessage] m, MonadParsec e Text m) =>
  PatternModded Spanned ->
  m (MatchData Spanned)
interpretModded PatternModded {pat, mod = Nothing} = interpretPatPiece $ spanValue pat
interpretModded PatternModded {pat, mod = Just (Spanned ModOptional _)} = try (interpretPatPiece $ spanValue pat) <|> pure MatchEmpty
interpretModded PatternModded {pat, mod = Just (Spanned ModMany _)} =
  MatchArray <$> try (nonEmptyP (pure ()) $ spanned $ dataToMatch $ interpretPatPiece $ spanValue pat) <|> pure MatchEmpty
interpretModded PatternModded {pat, mod = Just (Spanned ModSome _)} =
  MatchArray <$> (nonEmptyP (pure ()) $ spanned $ dataToMatch $ interpretPatPiece $ spanValue pat)
interpretModded PatternModded {mod = Just (Spanned (ModRepeat 0) _)} = pure MatchEmpty
interpretModded PatternModded {pat, mod = Just (Spanned (ModRepeat n) _)} =
  MatchArray . NE.fromList <$> sequenceA (replicate (fromInteger n) $ spanned $ dataToMatch $ interpretPatPiece $ spanValue pat)

interpretPatPiece ::
  ( MonadError DiagnosticMessage m
  , MonadWriter [DiagnosticMessage] m
  , MonadParsec e Text m
  ) =>
  PatternPiece Spanned ->
  m (MatchData Spanned)
interpretPatPiece (PText (StringLiteral s)) = MatchEmpty <$ string s
interpretPatPiece (PBind (Spanned BindDiscard _) (Spanned b _)) = MatchEmpty <$ interpretChoice Embedded b
interpretPatPiece (PBind (Spanned (BindId (Identifier i)) _) (Spanned b _)) =
  MatchRecord . Map.singleton i <$> spanned (interpret Embedded b)
interpretPatPiece (PBraced patChoice) = interpretChoice Embedded patChoice
interpretPatPiece (PIdent _ident) = do
  source <- getInput
  throwError $
    DiagnosticMessage
      source
      (Just "Referring to patterns by names is not yet implemented")
      []
