module Lux.Common.Parsers where

import Data.Char qualified as Char (isLetter)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Lux.Common.Types
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.SepBy qualified as SepBy (sepBy)
import Text.Megaparsec.Spanned (Spanned, spanned)

identifier :: (Token s ~ Char, MonadParsec e s m) => m Identifier
identifier = into $ some $ satisfy \c -> Char.isLetter c || c == '_'
  where
    into = fmap $ Identifier . tokensToChunk (Proxy @Text)

stringLiteral
  , stringLiteralDoubleQuote
  , stringLiteralSingleQuote ::
    (Token s ~ Char, MonadParsec e s m) => m StringLiteral
stringLiteral = try stringLiteralDoubleQuote <|> stringLiteralSingleQuote
stringLiteralDoubleQuote = char '"' *> stringLiteralInner '"' <* char '"'
stringLiteralSingleQuote = char '\'' *> stringLiteralInner '\'' <* char '\''

stringLiteralInner :: (Token s ~ Char, MonadParsec e s m) => Char -> m StringLiteral
stringLiteralInner sep = into $ some $ satisfy isStringLitChar
  where
    into = fmap $ StringLiteral . tokensToChunk (Proxy @Text)
    isStringLitChar = (/= sep)

fieldPath :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (FieldPath Spanned)
fieldPath = FieldPath <$> SepBy.sepBy (spanned $ FieldPathSep <$ char '.') (spanned field)

field :: (Token s ~ Char, MonadParsec e s m) => m Field
field =
  try (NamedField <$> identifier)
    <|> IndexField <$> decimal

curlyBraceOpen :: (Token s ~ Char, MonadParsec e s m) => m CurlyBraceOpen
curlyBraceOpen = CurlyBraceOpen <$ char '{'

curlyBraceClose :: (Token s ~ Char, MonadParsec e s m) => m CurlyBraceClose
curlyBraceClose = CurlyBraceClose <$ char '}'

squareBracketOpen :: (Token s ~ Char, MonadParsec e s m) => m SquareBracketOpen
squareBracketOpen = SquareBracketOpen <$ char '['

squareBracketClose :: (Token s ~ Char, MonadParsec e s m) => m SquareBracketClose
squareBracketClose = SquareBracketClose <$ char ']'

roundParenthesisOpen :: (Token s ~ Char, MonadParsec e s m) => m RoundParenthesisOpen
roundParenthesisOpen = RoundParenthesisOpen <$ char '('

roundParenthesisClose :: (Token s ~ Char, MonadParsec e s m) => m RoundParenthesisClose
roundParenthesisClose = RoundParenthesisClose <$ char ')'
