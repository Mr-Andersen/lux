module Lux.Common.Types where

import Data.Functor.Identity (Identity)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T (unpack)
import Text.Megaparsec.SepBy (SepBy)
import Text.Megaparsec.Spanned (Spanned)
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), text)

-- | ident = [A-Za-z][A-Za-z_]*
newtype Identifier = Identifier Text
  deriving newtype (Eq, IsString, Ord, Show)

instance Pretty Identifier where
  pPrint (Identifier i) = text $ T.unpack i

-- | string-literal = '"' string-literal-raw '"' | '\'' string-literal-raw  '\''
newtype StringLiteral = StringLiteral Text
  deriving newtype (Eq, IsString, Ord, Show)

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

-- {}

data CurlyBraceOpen = CurlyBraceOpen
  deriving stock (Eq)

instance Show CurlyBraceOpen where
  show CurlyBraceOpen = "\"{\""

data CurlyBraceClose = CurlyBraceClose
  deriving stock (Eq)

instance Show CurlyBraceClose where
  show CurlyBraceClose = "\"}\""

-- []

data SquareBracketOpen = SquareBracketOpen
  deriving stock (Eq)

instance Show SquareBracketOpen where
  show SquareBracketOpen = "\"[\""

data SquareBracketClose = SquareBracketClose
  deriving stock (Eq)

instance Show SquareBracketClose where
  show SquareBracketClose = "\"]\""

-- ()

data RoundParenthesisOpen = RoundParenthesisOpen
  deriving stock (Eq)

instance Show RoundParenthesisOpen where
  show RoundParenthesisOpen = "\"(\""

data RoundParenthesisClose = RoundParenthesisClose
  deriving stock (Eq)

instance Show RoundParenthesisClose where
  show RoundParenthesisClose = "\")\""
