module Lux.Output.Parsers where

import Lux.Common.Parsers (fieldPath, identifier, stringLiteral)
import Lux.Input.Parsers (choiceSep, sequenceSep)
import Lux.Output.Types
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.SepBy qualified as SepBy (sepBy)
import Text.Megaparsec.Spanned

outputChoice :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (OutputChoice Spanned)
outputChoice = OutputChoice <$> SepBy.sepBy (spanned choiceSep) (spanned outputSequence)

outputSequence :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (OutputSequence Spanned)
outputSequence = OutputSequence <$> SepBy.sepBy (spanned sequenceSep) (spanned outputValue)

outputValue :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (OutputValue Spanned)
outputValue =
  try (char '(' *> (OutputBraced <$> outputSequence) <* char ')')
    <|> try (OutputValue <$> spanned identifier <*> optional (char '.' *> spanned fieldPath))
    <|> OutputString <$> stringLiteral
