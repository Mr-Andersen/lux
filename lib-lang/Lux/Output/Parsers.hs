module Lux.Output.Parsers where

import Lux.Common.Parsers (identifier, stringLiteral)
import Lux.Output.Types
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Spanned
import Text.Megaparsec.SepBy qualified as SepBy (sepBy)
import Lux.Input.Parsers (choiceSep, sequenceSep)

outputChoice :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (OutputChoice Spanned)
outputChoice = OutputChoice <$> SepBy.sepBy (spanned choiceSep) (spanned outputSequence)

outputSequence :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (OutputSequence Spanned)
outputSequence = OutputSequence <$> SepBy.sepBy (spanned sequenceSep) (spanned outputValue)

outputValue :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (OutputValue Spanned)
outputValue =
  try (char '(' *> (OutputBraced <$> outputSequence) <* char ')')
    <|> try (OutputValue <$> spanned identifier <*> optional (char '.' *> spanned fieldPath))
    <|> OutputString <$> stringLiteral

fieldPath :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (FieldPath Spanned)
fieldPath = FieldPath <$> SepBy.sepBy (spanned $ FieldPathSep <$ char '.') (spanned field)

field :: (Token s ~ Char, MonadParsec e s m) => m Field
field =
  try (NamedField <$> identifier)
    <|> IndexField <$> decimal
