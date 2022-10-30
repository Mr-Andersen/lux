module Lux.Common.Types where

import Data.String (IsString)
import Data.Text (Text)

-- | ident = [A-Za-z][A-Za-z_]*
newtype Identifier = Identifier Text
  deriving newtype (Eq, IsString, Show)

-- | string-literal = '"' string-literal-raw '"'
newtype StringLiteral = StringLiteral Text
  deriving newtype (Eq, IsString, Show)
