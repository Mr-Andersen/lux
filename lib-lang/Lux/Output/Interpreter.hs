module Lux.Output.Interpreter where

import Control.Category ((>>>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE (length, (!!))
import Data.Map qualified as Map (lookup)
import Data.Text (Text)
import Data.Text qualified as T (pack)
import Lux.Common.Types (Identifier (Identifier), StringLiteral (StringLiteral))
import Lux.Input.Match (Match (Match, data_, raw), MatchData (MatchArray, MatchRecord))
import Lux.Input.Types (ChoiceSep)
import Lux.Output.Types
import Text.Megaparsec.SepBy (SepBy ((:#)))
import Text.Megaparsec.Spanned (Spanned (Spanned, value))

newtype MissingField = MissingField Text -- TODO: need to use DiagnosticMessage here

resultTryer :: Semigroup e => Either e a -> Either e a -> Either e a
resultTryer (Left errs) y = first (errs <>) y
resultTryer ok _ = ok

foldSepBy' :: forall sep a b. (a -> b) -> (b -> b -> b) -> SepBy sep a -> b
foldSepBy' f folder (h :# t) = foldl1 folder (fmap f $ h : fmap snd t)

foldSepBy :: forall sep a. (a -> sep -> a -> a) -> SepBy sep a -> a
foldSepBy folder (h :# t) =
  case t of
    [] -> h
    (sep, x) : xs -> foldSepBy folder (folder h sep x :# xs)

interpret :: Match Spanned -> OutputChoice Spanned -> Either [MissingField] Text
interpret m = foldSepBy' @(Spanned ChoiceSep) (interpretSeq m . value) resultTryer . coerce

interpretSeq :: Match Spanned -> OutputSequence Spanned -> Either [MissingField] Text
interpretSeq m = coerce >>> fmap (interpretValue m . value) >>> foldSepBy folder
  where
    -- TODO: should use Applicative on Result
    folder (Right x) (Spanned sep _) (Right y) = pure $ x <> sep' <> y
      where
        sep' = case sep of
          SepComma -> mempty
          SepSpace -> " "
    folder (Left e) _ (Left e') = Left $ e <> e'
    folder (Left e) _ _ = Left e
    folder _ _ (Left e) = Left e

interpretValue :: Match Spanned -> OutputValue Spanned -> Either [MissingField] Text
interpretValue m (OutputBraced s) = interpretSeq m s
interpretValue _ (OutputString (StringLiteral s)) = pure s
interpretValue (Match _ (MatchRecord m)) OutputValue {input = Spanned (Identifier i) _, fieldPath} =
  Map.lookup i m & maybe (Left [MissingField i]) (value >>> flip interpretFieldPath fieldPath)
interpretValue _ OutputValue {input = Spanned (Identifier i) _} = Left [MissingField i]

interpretFieldPath :: Match Spanned -> Maybe (Spanned (FieldPath Spanned)) -> Either [MissingField] Text
interpretFieldPath Match {raw = Spanned r _} Nothing = pure r
interpretFieldPath Match {data_} (Just (Spanned (FieldPath (Spanned h _ :# t)) _)) =
  case (data_, h) of
    (MatchRecord m, NamedField (Identifier i)) ->
      Map.lookup i m & maybe (Left [MissingField i]) (value >>> flip interpretField t)
    (MatchArray a, IndexField (Index i))
      | NE.length a > fromInteger i ->
          a NE.!! fromInteger i & value & flip interpretField t
    (_, NamedField (Identifier i)) -> Left [MissingField i]
    (_, IndexField (Index i)) -> Left [MissingField $ T.pack $ show i]

interpretField :: Match Spanned -> [(Spanned FieldPathSep, Spanned Field)] -> Either [MissingField] Text
interpretField Match {raw} [] = pure $ value raw
interpretField Match {data_} ((_, Spanned f _) : fs) =
  case (data_, f) of
    (MatchRecord m, NamedField (Identifier i)) ->
      Map.lookup i m & maybe (Left [MissingField i]) (value >>> flip interpretField fs)
    (MatchArray a, IndexField (Index i))
      | NE.length a > fromInteger i ->
          a NE.!! fromInteger i & value & flip interpretField fs
    (_, NamedField (Identifier i)) -> Left [MissingField i]
    (_, IndexField (Index i)) -> Left [MissingField $ T.pack $ show i]
