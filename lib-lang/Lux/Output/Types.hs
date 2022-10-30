module Lux.Output.Types where

import Data.Functor.Identity (Identity)
import Lux.Common.Types (Identifier, StringLiteral)
import Text.Megaparsec.Spanned (Spanned)
import Text.Megaparsec.SepBy (SepBy)
import Lux.Input.Types (ChoiceSep, SequenceSep)

type Output = OutputChoice

-- | output-choice = output-sequence ['|' output-choice]
newtype OutputChoice span = OutputChoice
  { val :: SepBy (span ChoiceSep) (span (OutputSequence span))
  }

deriving stock instance Eq (OutputChoice Identity)
deriving stock instance Show (OutputChoice Identity)
deriving stock instance Eq (OutputChoice Spanned)
deriving stock instance Show (OutputChoice Spanned)

-- | output-sequence = output-value [spaces1 output-sequence]
newtype OutputSequence span = OutputSequence
  { val :: SepBy (span SequenceSep) (span (OutputValue span))
  }

deriving stock instance Eq (OutputSequence Identity)
deriving stock instance Show (OutputSequence Identity)
deriving stock instance Eq (OutputSequence Spanned)
deriving stock instance Show (OutputSequence Spanned)

{- |
output-value
  = '(' output-sequence ')'
  | ident ['.' field-path]
-}
data OutputValue span
  = OutputBraced (OutputSequence span)
  | OutputValue
      { input :: span Identifier
      , fieldPath :: Maybe (span (FieldPath span))
      }
  | OutputString StringLiteral

deriving stock instance Eq (OutputValue Identity)
deriving stock instance Show (OutputValue Identity)
deriving stock instance Eq (OutputValue Spanned)
deriving stock instance Show (OutputValue Spanned)

-- | field-path = field ['.' field-path]
newtype FieldPath span = FieldPath
  { val :: SepBy (span FieldPathSep) (span Field)
  }

deriving stock instance Eq (FieldPath Identity)
deriving stock instance Show (FieldPath Identity)
deriving stock instance Eq (FieldPath Spanned)
deriving stock instance Show (FieldPath Spanned)

data FieldPathSep = FieldPathSep
  deriving stock (Eq, Show)

-- | field = ident | index
data Field
  = NamedField Identifier
  | IndexField Index
  deriving stock (Eq, Show)

-- | index = decimal
newtype Index = Index Integer
  deriving newtype (Eq, Num, Show)
