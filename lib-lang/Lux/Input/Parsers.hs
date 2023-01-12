module Lux.Input.Parsers where

import Data.Coerce (coerce)
import Lux.Common.Parsers (curlyBraceClose, curlyBraceOpen, fieldPath, identifier, roundParenthesisClose, roundParenthesisOpen, squareBracketClose, squareBracketOpen, stringLiteral)
import Lux.Common.Types (StringLiteral (StringLiteral))
import Lux.Input.Types
import Text.Megaparsec hiding (sepBy)
import Text.Megaparsec.Braced (braced)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.SepBy (SepBy ((:#)), sepBy)
import Text.Megaparsec.Spanned

pattern :: forall e s m. (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (Pattern Spanned)
pattern = shallowShrinkPattern . PChoice <$> sepBy (spanned choiceSep) (spanned psequence)
  where
    psequence = shallowShrinkPattern . PSequence <$> sepBy (spanned sequenceSep) (spanned (try (PModded <$> pmodded) <|> pterminal))
    -- fmap (shallowShrinkPattern . PSequence) $
    --   (:#)
    --     <$> spanned (try (PModded <$> pmodded) <|> pterminal)
    --     <*> many ((,) <$> spanned sequenceSep <*> spanned pattern)
    pterminal =
      try (PIdent <$> identifier)
        <|> try (PLiteral . coerce <$> stringLiteral)
        <|> try (PPatternPun <$> braced (spanned curlyBraceOpen) (spanned curlyBraceClose) (spanned identifier))
        <|> try (PBindRecord <$> braced (spanned curlyBraceOpen) (spanned curlyBraceClose) (spanned bindRaw))
        <|> try (PPushArray <$> braced (spanned squareBracketOpen) (spanned squareBracketClose) (spanned pattern))
        <|> PBraced <$> braced (spanned roundParenthesisOpen) (spanned roundParenthesisClose) (spanned pattern)
    pmodded = Modded <$> spanned pterminal <*> spanned patternModifier
    bindRaw = BindRaw <$> spanned bindIdentifier <*> spanned bindSeparator <*> spanned pattern

shallowShrinkPattern :: Pattern Spanned -> Pattern Spanned
shallowShrinkPattern (PChoice (h :# [])) = h.value
shallowShrinkPattern (PSequence (h :# [])) = h.value
shallowShrinkPattern p = p

choiceSep :: (Token s ~ Char, MonadParsec e s m) => m ChoiceSep
choiceSep = ChoiceSep <$ char '|'

sequenceSep :: (Token s ~ Char, MonadParsec e s m) => m SequenceSep
sequenceSep =
  try (SequenceSpace <$ char ' ')
    <|> try (SequenceConcat <$ char ',')
    <|> try (SequenceChar <$> (char '\\' *> anySingle) <?> "raw sequence separator")
    <|> pure SequenceConcat

patternModifier :: (Token s ~ Char, MonadParsec e s m) => m PatternModifier
patternModifier =
  try (ModMany <$ char '*')
    <|> try (ModSome <$ char '+')
    <|> try (ModOptional <$ char '?')
    <|> ModRepeat <$> (char '{' *> decimal <* char '}')

bindIdentifier :: (Token s ~ Char, TraversableStream s, MonadParsec e s m) => m (BindIdentifier Spanned)
bindIdentifier =
  try (BindDiscard <$ char '_')
    <|> BindId <$> fieldPath

bindSeparator :: (Token s ~ Char, MonadParsec e s m) => m BindSep
bindSeparator = BindSep <$ char ':'
