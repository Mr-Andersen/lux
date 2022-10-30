module Lux.Input.Parsers where

import Lux.Common.Parsers (identifier, stringLiteral)
import Lux.Input.Types
import Text.Megaparsec hiding (sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.SepBy (sepBy)
import Text.Megaparsec.SepBy qualified as SepBy (singleton)
import Text.Megaparsec.Spanned

patternChoice :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (PatternChoice Spanned)
patternChoice = PatternChoice <$> sepBy (spanned choiceSep) (spanned patternSequence)

choiceSep :: (Token s ~ Char, MonadParsec e s m) => m ChoiceSep
choiceSep = ChoiceSep <$ char '|'

patternSequence :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (PatternSequence Spanned)
patternSequence =
  PatternSequence <$> sepBy (spanned sequenceSep) (spanned patternModded)

sequenceSep :: (Token s ~ Char, MonadParsec e s m) => m SequenceSep
sequenceSep = try (SepSpace <$ char ' ') <|> SepComma <$ char ','

patternModded :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (PatternModded Spanned)
patternModded =
  PatternModded <$> spanned patternPiece <*> optional (spanned patternModifier)

patternPiece :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (PatternPiece Spanned)
patternPiece =
  try (PText <$> stringLiteral)
    <|> try (char '(' *> patternBind <* char ')')
    <|> try (char '(' *> patternPunBind <* char ')')
    <|> try (char '(' *> (PBraced <$> patternChoice) <* char ')')
    <|> PIdent <$> identifier

patternBind :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (PatternPiece Spanned)
patternBind = PBind <$> spanned bindIdentifier <* char ':' <*> spanned patternChoice

patternPunBind :: (TraversableStream s, Token s ~ Char, MonadParsec e s m) => m (PatternPiece Spanned)
patternPunBind = unPun <$> spanned identifier
  where
    unPun i =
      let v = spanAs i $ BindId $ spanValue i
          -- p = spanAs i $ PatternChoice (spanAs i $ PatternSequence (spanAs i $ PatternModded (spanAs i $ PIdent $ spanValue i) Nothing) Nothing) Nothing
          p =
            spanAs i $
              PatternChoice $
                SepBy.singleton $
                  spanAs i $
                    PatternSequence $
                      SepBy.singleton $
                        spanAs i $
                          PatternModded (spanAs i $ PIdent $ spanValue i) Nothing
       in PBind v p

patternModifier :: (Token s ~ Char, MonadParsec e s m) => m PatternModifier
patternModifier =
  try (ModMany <$ char '*')
    <|> try (ModSome <$ char '+')
    <|> try (ModOptional <$ char '?')
    <|> ModRepeat <$> (char '{' *> decimal <* char '}')

bindIdentifier :: (Token s ~ Char, MonadParsec e s m) => m BindIdentifier
bindIdentifier =
  try (BindDiscard <$ char '_')
    <|> BindId <$> identifier
