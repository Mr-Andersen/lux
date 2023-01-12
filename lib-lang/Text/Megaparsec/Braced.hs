module Text.Megaparsec.Braced where

import Text.Megaparsec (MonadParsec)

data Braced left right a = Braced
  { left :: left
  , right :: right
  , value :: a
  }
  deriving stock (Eq, Show)

-- instance (Show left, Show right, Show a) => Show (Braced left right a) where
--   show Braced{left, right, value} = show left <> show value <> show right

braced :: MonadParsec e s m => m left -> m right -> m a -> m (Braced left right a)
braced pleft pright pvalue = do
  left <- pleft
  value <- pvalue
  right <- pright
  pure Braced {left, right, value}
