module Lux.Common.Parsers where

import Data.Char qualified as Char (isLetter)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Lux.Common.Types
import Text.Megaparsec
import Text.Megaparsec.Char (char)

identifier :: (Token s ~ Char, MonadParsec e s m) => m Identifier
identifier = into $ (:) <$> satisfy Char.isLetter <*> many (satisfy \c -> Char.isLetter c || c == '_')
  where
    into = fmap $ Identifier . tokensToChunk (Proxy @Text)

stringLiteral
  , stringLiteralDoubleQuote
  , stringLiteralSingleQuote ::
    (Token s ~ Char, MonadParsec e s m) => m StringLiteral
stringLiteral = try stringLiteralDoubleQuote <|> stringLiteralSingleQuote
stringLiteralDoubleQuote = char '"' *> into stringLiteralInner <* char '"'
  where
    into = fmap $ StringLiteral . tokensToChunk (Proxy @Text)
    stringLiteralInner = some (satisfy isStringLitChar)
    isStringLitChar = (/= '"')
stringLiteralSingleQuote = char '\'' *> into stringLiteralInner <* char '\''
  where
    into = fmap $ StringLiteral . tokensToChunk (Proxy @Text)
    stringLiteralInner = some (satisfy isStringLitChar)
    isStringLitChar = (/= '\'')
