module Lux.Input.Match where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Merge.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (foldlWithKey, null)
import Data.Text (Text)
import Data.Text qualified as T (unpack)
import Lux.UnSpan (UnSpan (unSpan), spannedToId, unSpan')
import Text.Megaparsec
import Text.Megaparsec.Diagnostic
import Text.Megaparsec.Spanned
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), text)
import Utils (intersperseM)

data Match span = Match
  -- { _matchSource :: Pattern Spanned
  { raw :: span Text
  -- ^ value that is returned by using variable directly
  -- e.g. `(a:date) => (a)` matching on `"2022-01-01"` will return `"2022-01-01"`
  , data_ :: MatchData span
  }

instance UnSpan Match where
  unSpan (Match r d) = Match (spannedToId r) (unSpan d)

instance UnSpan MatchData where
  unSpan MatchEmpty = MatchEmpty
  unSpan (MatchRecord rec) = MatchRecord $ rec <&> unSpan'
  unSpan (MatchArray arr) = MatchArray $ arr <&> unSpan'

deriving stock instance Eq (Match Identity)
deriving stock instance Show (Match Identity)
deriving stock instance Eq (Match Spanned)
deriving stock instance Show (Match Spanned)

-- TODO: top-level matches and inner match data are different (use GADT)
data MatchData span
  = MatchEmpty
  | MatchRecord (Map Text (span (Match span)))
  | MatchArray (NonEmpty (span (Match span)))

deriving stock instance Eq (MatchData Identity)
deriving stock instance Show (MatchData Identity)
deriving stock instance Eq (MatchData Spanned)
deriving stock instance Show (MatchData Spanned)

dataToMatch :: (Tokens s ~ Text, TraversableStream s, MonadParsec e s m) => m (MatchData Spanned) -> m (Match Spanned)
dataToMatch pdata = match (spanned pdata) <&> \(src, Spanned data_ sp) -> Match (Spanned src sp) data_

instance Pretty (Match Identity) where
  pPrint (Match (Identity r) d) = "\"" <> text (T.unpack r) <> "\"" <> pPrint d

instance Pretty (MatchData Identity) where
  pPrint MatchEmpty = mempty
  pPrint (MatchRecord rec) =
    "{"
      <> intersperseM "," (Map.foldlWithKey (\a k (Identity v) -> a <> [text (T.unpack k) <> ":" <> pPrint v]) [] rec)
      <> "}"
  pPrint (MatchArray arr) =
    "["
      <> intersperseM "," (NE.toList $ pPrint . runIdentity <$> arr)
      <> "]"

mergeData ::
  (MonadError DiagnosticMessage m, MonadWriter [DiagnosticMessage] m, MonadParsec e Text m) =>
  MatchData Spanned ->
  MatchData Spanned ->
  m (MatchData Spanned)
mergeData (MatchRecord rec) (MatchRecord rec') =
  MatchRecord
    <$> mergeA
      preserveMissing
      preserveMissing
      ( zipWithAMatched \_ v v' ->
          v' <$ diagnostic (Just "Repeating key in a record (preferring second value)")
                  [ spanAs v "first value"
                  , spanAs v' "second value"
                  ]
      )
      rec
      rec'
mergeData (MatchArray xs) (MatchArray ys) = pure $ MatchArray (xs <> ys)
mergeData x MatchEmpty = pure x
mergeData MatchEmpty x = pure x
mergeData (MatchRecord rec) other | Map.null rec = pure other
mergeData x y =
    diagnosticThrow
      (Just $ "Merging incompatible data: " <> pPrint (unSpan x) <> ", " <> pPrint (unSpan y))
      []
