module Lux.Output.Interpret where

import Control.Category ((>>>))
import Data.Bifunctor (Bifunctor(bimap))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Map qualified as Map (lookup)
import Data.List.NonEmpty qualified as NE (length, (!!))
import Data.Text (Text)
import Data.Text qualified as T (pack)
import Lux.Common.Types (StringLiteral (StringLiteral), Identifier (Identifier))
import Lux.Input.Match (Match(Match, raw, data_), MatchData (MatchRecord, MatchArray))
import Lux.Input.Types (ChoiceSep, SequenceSep (SepComma, SepSpace))
import Lux.Output.Types
import Text.Megaparsec.SepBy (SepBy((:#)))
import Text.Megaparsec.Spanned (Spanned (spanValue, Spanned))

newtype MissingField = MissingField Text -- TODO: need to use DiagnosticMessage here

resultTryer :: Semigroup e => Either e a -> Either e a -> Either e a
resultTryer (Left errs) y = bimap (errs <>) id $ y
resultTryer ok _ = ok

foldSepBy' :: forall sep a b. (a -> b) -> (b -> b -> b) -> SepBy sep a -> b
foldSepBy' f folder (h :# t) = foldl1 folder (fmap f $ h : fmap snd t)

foldSepBy :: forall sep a. (a -> sep -> a -> a) -> SepBy sep a -> a
foldSepBy folder (h :# t) =
  case t of
    [] -> h
    (sep, x):xs -> foldSepBy folder (folder h sep x :# xs)

interpret :: Match Spanned -> OutputChoice Spanned -> Either [MissingField] Text
interpret m = foldSepBy' @(Spanned ChoiceSep) (interpretSeq m . spanValue) resultTryer . coerce

interpretSeq :: Match Spanned -> OutputSequence Spanned -> Either [MissingField] Text
interpretSeq m = coerce >>> fmap (interpretValue m . spanValue) >>> foldSepBy folder where
  -- TODO: should use Applicative on Result
  folder (Right x) (Spanned sep _) (Right y) = pure $ x <> sep' <> y where
    sep' = case sep of
      SepComma -> mempty
      SepSpace -> " "
  folder (Left e) _ (Left e') = Left $ e <> e'
  folder (Left e) _ _ = Left e
  folder _ _ (Left e) = Left e

interpretValue :: Match Spanned -> OutputValue Spanned -> Either [MissingField] Text
interpretValue m (OutputBraced s) = interpretSeq m s
interpretValue _ (OutputString (StringLiteral s)) = pure s
interpretValue (Match _ (MatchRecord m)) OutputValue{input = Spanned (Identifier i) _, fieldPath} =
  Map.lookup i m & maybe (Left [MissingField i]) (spanValue >>> flip interpretFieldPath fieldPath)
interpretValue _ OutputValue{input = Spanned (Identifier i) _} = Left [MissingField i]

interpretFieldPath :: Match Spanned -> Maybe (Spanned (FieldPath Spanned)) -> Either [MissingField] Text
interpretFieldPath Match{raw = Spanned r _} Nothing = pure r
interpretFieldPath Match{data_} (Just (Spanned (FieldPath (Spanned h _ :# t)) _)) =
  case (data_, h) of
    (MatchRecord m, NamedField (Identifier i)) ->
      Map.lookup i m & maybe (Left [MissingField i]) (spanValue >>> flip interpretField t)
    (MatchArray a, IndexField (Index i)) | NE.length a > fromInteger i ->
      a NE.!! fromInteger i & spanValue & flip interpretField t
    (_, NamedField (Identifier i)) -> Left [MissingField i]
    (_, IndexField (Index i)) -> Left [MissingField $ T.pack $ show i]

interpretField :: Match Spanned -> [(Spanned FieldPathSep, Spanned Field)] -> Either [MissingField] Text
interpretField Match{raw} [] = pure $ spanValue raw
interpretField Match{data_} ((_, Spanned f _):fs) =
  case (data_, f) of
    (MatchRecord m, NamedField (Identifier i)) ->
      Map.lookup i m & maybe (Left [MissingField i]) (spanValue >>> flip interpretField fs)
    (MatchArray a, IndexField (Index i)) | NE.length a > fromInteger i ->
      a NE.!! fromInteger i & spanValue & flip interpretField fs
    (_, NamedField (Identifier i)) -> Left [MissingField i]
    (_, IndexField (Index i)) -> Left [MissingField $ T.pack $ show i]
