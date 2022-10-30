module Lux.UnSpan where

import Data.Bifunctor (bimap)
import Data.Functor.Identity (Identity (Identity))
import Lux.Input.Types
import Lux.Output.Types
import Text.Megaparsec.SepBy (SepBy ((:#)))
import Text.Megaparsec.Spanned (Spanned, spanValue)

class UnSpan f where
  unSpan :: f Spanned -> f Identity

unSpan' :: UnSpan f => Spanned (f Spanned) -> Identity (f Identity)
unSpan' = Identity . unSpan . spanValue

spannedToId :: Spanned a -> Identity a
spannedToId = Identity . spanValue

newtype SepBySpanned sep f span = SepBySpanned {val :: SepBy (span sep) (span (f span))}

instance UnSpan f => UnSpan (SepBySpanned sep f) where
  unSpan SepBySpanned {val = h :# t} = SepBySpanned $ unSpan' h :# fmap (bimap spannedToId unSpan') t

newtype SepBySpanned2 sep a span = SepBySpanned2 {val :: SepBy (span sep) (span a)}

instance UnSpan (SepBySpanned2 sep a) where
  unSpan SepBySpanned2 {val = h :# t} = SepBySpanned2 $ spannedToId h :# fmap (bimap spannedToId spannedToId) t

deriving via SepBySpanned ChoiceSep PatternSequence instance UnSpan PatternChoice

deriving via SepBySpanned SequenceSep PatternModded instance UnSpan PatternSequence

instance UnSpan PatternModded where
  unSpan (PatternModded p m) = PatternModded (unSpan' p) (fmap spannedToId m)

instance UnSpan PatternPiece where
  unSpan (PText s) = PText s
  unSpan (PBind v b) = PBind (spannedToId v) (unSpan' b)
  unSpan (PBraced p) = PBraced (unSpan p)
  unSpan (PIdent i) = PIdent i

deriving via SepBySpanned ChoiceSep OutputSequence instance UnSpan OutputChoice

deriving via SepBySpanned SequenceSep OutputValue instance UnSpan OutputSequence

instance UnSpan OutputValue where
  unSpan (OutputBraced s) = OutputBraced (unSpan s)
  unSpan (OutputValue i fp) = OutputValue (spannedToId i) (fmap unSpan' fp)
  unSpan (OutputString s) = OutputString s

deriving via SepBySpanned2 FieldPathSep Field instance UnSpan FieldPath
