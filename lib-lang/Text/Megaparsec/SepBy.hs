module Text.Megaparsec.SepBy where

import Data.Bifoldable (Bifoldable (bifoldr))
import Data.Bifunctor
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Text.Megaparsec
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), comma)
import Utils (intersperseM)

-- | Non-empty list of tokens `a` separated by `sep`
data SepBy sep a = a :# [(sep, a)]
  deriving stock (Eq, Show)

instance Functor (SepBy sep) where
  fmap = bimap id

instance Bifunctor SepBy where
  bimap fsep fa (h :# t) = fa h :# fmap (bimap fsep fa) t

instance Foldable (SepBy sep) where
  -- foldr f i (h :# t) = foldr f (f h i) $ snd <$> t
  foldr = bifoldr (const id)

instance Bifoldable SepBy where
  -- bifoldr f g i (h :# t) = foldr (\(sep, x) -> g x . f sep) (g h i) t
  bifoldr f g i (h :# t) = foldr (flip $ bifoldr f g) (g h i) t

instance Traversable (SepBy sep) where
  -- traverse f (h :# t) = (:#) <$> f h <*> traverse (\(sep, a) -> (sep,) <$> f a) t
  traverse = bitraverse pure

instance Bitraversable SepBy where
  bitraverse f g (h :# t) = (:#) <$> g h <*> traverse (bitraverse f g) t

singleton :: a -> SepBy sep a
singleton a = a :# []

head :: SepBy sep a -> a
head (h :# _) = h

tail :: SepBy sep a -> [(sep, a)]
tail (_ :# t) = t

instance (Pretty sep, Pretty a) => Pretty (SepBy sep a) where
  pPrint (h :# t) =
    "["
      <> intersperseM comma (pPrint h : foldMap (\(sep, a) -> [pPrint sep, pPrint a]) t)
      <> "]"

sepBy :: MonadParsec e s m => m sep -> m a -> m (SepBy sep a)
sepBy sep x = (:#) <$> x <*> many ((,) <$> sep <*> x)

values :: SepBy s a -> NonEmpty a
values (h :# t) = h :| fmap snd t
