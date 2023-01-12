module Lux.Input.Types where

-- import Data.Functor.Identity (Identity)

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Text (Text)
import Lux.Common.Types
import Text.Megaparsec.Braced (Braced)
import Text.Megaparsec.SepBy (SepBy)
import Text.Megaparsec.Spanned (Spanned)

type Pattern :: (Type -> Type) -> Type
data Pattern span
  = PIdent Identifier
  | PLiteral Text
  | -- | @'{' identifier \'}\'@
    PPatternPun (Braced (span CurlyBraceOpen) (span CurlyBraceClose) (span Identifier))
  | -- | @pattern pattern-modifier@
    PModded (Modded span)
  | -- | @'{' bind-raw \'}\'@
    PBindRecord (Braced (span CurlyBraceOpen) (span CurlyBraceClose) (span (BindRaw span)))
  | -- | @'[' pattern pattern-modifier \']\'@
    PPushArray (Braced (span SquareBracketOpen) (span SquareBracketClose) (span (Pattern span)))
  | -- | @pattern (sequence-separator pattern)*@
    PSequence (SepBy (span SequenceSep) (span (Pattern span)))
  | -- | @pattern (choice-separator pattern)*@
    PChoice (SepBy (span ChoiceSep) (span (Pattern span)))
  | -- | @'(' pattern \')\'@
    PBraced (Braced (span RoundParenthesisOpen) (span RoundParenthesisClose) (span (Pattern span)))

deriving stock instance Eq (Pattern Identity)
deriving stock instance Show (Pattern Identity)
deriving stock instance Eq (Pattern Spanned)
deriving stock instance Show (Pattern Spanned)

-- | @variable \':\' pattern@
data BindRaw span = BindRaw
  { varName :: span (BindIdentifier span)
  , sep :: span BindSep
  , pat :: span (Pattern span)
  }

deriving stock instance Eq (BindRaw Identity)
deriving stock instance Show (BindRaw Identity)
deriving stock instance Eq (BindRaw Spanned)
deriving stock instance Show (BindRaw Spanned)

-- | @pattern pattern-modifier@
data Modded span = Modded
  { pat :: span (Pattern span)
  , modifier :: span PatternModifier
  }

deriving stock instance Eq (Modded Identity)
deriving stock instance Show (Modded Identity)
deriving stock instance Eq (Modded Spanned)
deriving stock instance Show (Modded Spanned)

-- | @bind-separator = \':\'@
data BindSep = BindSep deriving stock (Eq, Show)

-- | @choice-separator = \'|\'@
data ChoiceSep = ChoiceSep deriving stock (Eq, Show)

-- | @pattern-separator = \' \' | \',' | \'\' | \'\\\' char@
data SequenceSep = SequenceSpace | SequenceConcat | SequenceChar Char
  deriving stock (Eq, Show)

-- | @pattern-modifier = \'*' | \'+' | \'?' | \'{' decimal \'}'@
data PatternModifier
  = ModMany
  | ModSome
  | ModOptional
  | ModRepeat Int
  deriving stock (Eq, Show)

-- | @bind-identifier = \'_\' | ident@
data BindIdentifier span
  = -- | @\'_\'@
    BindDiscard
  | -- | @ident@
    BindId (FieldPath span)

deriving stock instance Eq (BindIdentifier Identity)
deriving stock instance Show (BindIdentifier Identity)
deriving stock instance Eq (BindIdentifier Spanned)
deriving stock instance Show (BindIdentifier Spanned)
