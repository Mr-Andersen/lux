module Text.Megaparsec.NonEmpty (
  module Data.List.NonEmpty,
  nonEmptyP,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Text.Megaparsec

-- | @nonEmptyP sep val@ parses non-empty sequence of @val@s separated by @sep@s
nonEmptyP :: MonadParsec e s m => m () -> m a -> m (NonEmpty a)
nonEmptyP sep x = (:|) <$> x <*> many (sep *> x)
