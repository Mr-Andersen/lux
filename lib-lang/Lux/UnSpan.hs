module Lux.UnSpan where

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (Identity))
import Lux.Common.Types
import Lux.Input.Types
import Lux.Output.Types
import Text.Megaparsec.Braced (Braced (Braced, left, right, value))
import Text.Megaparsec.SepBy (SepBy ((:#)))
import Text.Megaparsec.Spanned (Spanned, value)

class UnSpan f where
  unSpan :: f Spanned -> f Identity

unSpan' :: UnSpan f => Spanned (f Spanned) -> Identity (f Identity)
unSpan' = Identity . unSpan . (.value)

spannedToId :: Spanned a -> Identity a
spannedToId = Identity . (.value)

newtype SepBySpanned sep f span = SepBySpanned {val :: SepBy (span sep) (span (f span))}

instance UnSpan f => UnSpan (SepBySpanned sep f) where
  unSpan SepBySpanned {val = h :# t} = SepBySpanned $ unSpan' h :# fmap (bimap spannedToId unSpan') t

newtype SimpleSepBySpanned sep a span = SimpleSepBySpanned {val :: SepBy (span sep) (span a)}

instance UnSpan (SimpleSepBySpanned sep a) where
  unSpan SimpleSepBySpanned {val = h :# t} = SimpleSepBySpanned $ spannedToId h :# fmap (bimap spannedToId spannedToId) t

instance UnSpan Pattern where
  unSpan (PIdent i) = PIdent i
  unSpan (PLiteral i) = PLiteral i
  unSpan (PPatternPun braced) = PPatternPun $ unSpanBraced braced
  unSpan (PModded (Modded {pat, modifier})) =
    PModded
      Modded
        { pat = unSpan' pat
        , modifier = spannedToId modifier
        }
  unSpan (PBindRecord braced) = PBindRecord $ unSpanBracedF braced
  unSpan (PPushArray braced) = PPushArray $ unSpanBracedF braced
  unSpan (PSequence s) = PSequence $ coerce $ unSpan $ SepBySpanned s
  unSpan (PChoice s) = PChoice $ coerce $ unSpan $ SepBySpanned s
  unSpan (PBraced braced) = PBraced $ unSpanBracedF braced

unSpanBracedF :: UnSpan f => Braced (Spanned left) (Spanned right) (Spanned (f Spanned)) -> Braced (Identity left) (Identity right) (Identity (f Identity))
unSpanBracedF Braced {left, right, value} = Braced {left = spannedToId left, right = spannedToId right, value = unSpan' value}

unSpanBraced :: Braced (Spanned left) (Spanned right) (Spanned a) -> Braced (Identity left) (Identity right) (Identity a)
unSpanBraced Braced {left, right, value} = Braced {left = spannedToId left, right = spannedToId right, value = spannedToId value}

instance UnSpan BindIdentifier where
  unSpan BindDiscard = BindDiscard
  unSpan (BindId fp) = BindId $ unSpan fp

instance UnSpan BindRaw where
  unSpan BindRaw {varName, sep, pat} = BindRaw {varName = unSpan' varName, sep = spannedToId sep, pat = unSpan' pat}

instance UnSpan Modded where
  unSpan Modded {pat, modifier} = Modded {pat = unSpan' pat, modifier = spannedToId modifier}

deriving via SepBySpanned ChoiceSep OutputSequence instance UnSpan OutputChoice

deriving via SepBySpanned SequenceSep OutputValue instance UnSpan OutputSequence

instance UnSpan OutputValue where
  unSpan (OutputBraced s) = OutputBraced (unSpan s)
  unSpan (OutputValue i fp) = OutputValue (spannedToId i) (fmap unSpan' fp)
  unSpan (OutputString s) = OutputString s

deriving via SimpleSepBySpanned FieldPathSep Field instance UnSpan FieldPath
