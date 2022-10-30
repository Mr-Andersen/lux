module Lux.Input.Types where

import Data.Functor.Identity (Identity)
import Lux.Common.Types (Identifier, StringLiteral)
import Text.Megaparsec.SepBy (SepBy)
import Text.Megaparsec.Spanned (Spanned)

type Pattern = PatternChoice

-- | pattern-choice = pattern-sequence ['|' pattern-choice]]
newtype PatternChoice span = PatternChoice
  { val :: SepBy (span ChoiceSep) (span (PatternSequence span))
  }

deriving stock instance Eq (PatternChoice Identity)
deriving stock instance Show (PatternChoice Identity)
deriving stock instance Eq (PatternChoice Spanned)
deriving stock instance Show (PatternChoice Spanned)

data ChoiceSep = ChoiceSep deriving stock (Eq, Show)

-- | pattern-sequence = pattern-modded [pattern-separator pattern-sequence]
newtype PatternSequence span = PatternSequence
  { val :: SepBy (span SequenceSep) (span (PatternModded span))
  }

deriving stock instance Eq (PatternSequence Identity)
deriving stock instance Show (PatternSequence Identity)
deriving stock instance Eq (PatternSequence Spanned)
deriving stock instance Show (PatternSequence Spanned)

-- | pattern-separator = ' ' | ','
data SequenceSep = SepSpace | SepComma
  deriving stock (Eq, Show)

-- | pattern-modded = pattern-piece [pattern-modifier]
data PatternModded span = PatternModded
  { pat :: span (PatternPiece span)
  , mod :: Maybe (span PatternModifier)
  }

deriving stock instance Eq (PatternModded Identity)
deriving stock instance Show (PatternModded Identity)
deriving stock instance Eq (PatternModded Spanned)
deriving stock instance Show (PatternModded Spanned)

{- |
pattern-piece
    = string-literal
    | '(' bind-identifier ':' pattern ')'
    | '(' pattern ')'
    | ident
-}
data PatternPiece span
  = PText StringLiteral
  | PBind {varName :: span BindIdentifier, bindPat :: span (PatternChoice span)}
  | PBraced (PatternChoice span)
  | PIdent Identifier

deriving stock instance Eq (PatternPiece Identity)
deriving stock instance Show (PatternPiece Identity)
deriving stock instance Eq (PatternPiece Spanned)
deriving stock instance Show (PatternPiece Spanned)

-- | pattern-modifier = '*' | '+' | '?' | '{' decimal '}'
data PatternModifier
  = ModMany
  | ModSome
  | ModOptional
  | ModRepeat Integer
  deriving stock (Eq, Show)

-- | bind-identifier = '_' | ident
data BindIdentifier
  = BindId Identifier
  | BindDiscard
  deriving stock (Eq, Show)
