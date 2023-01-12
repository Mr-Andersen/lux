module Text.Megaparsec.Spanned where

import Data.List.NonEmpty
import Text.Megaparsec

data Spanned a = Spanned {value :: a, spans :: NonEmpty Span}
  deriving stock (Eq, Functor, Ord, Show)

data Span = Span {spanStart :: SourcePos, spanEnd :: SourcePos}
  deriving stock (Eq, Ord, Show)

insertPartialOrd :: (a -> a -> Maybe Ordering) -> (a -> a -> a) -> a -> [a] -> [a]
insertPartialOrd _cmp _merge x [] = [x]
insertPartialOrd cmp merge x (y : ys) =
  case cmp x y of
    Nothing -> insertPartialOrd cmp merge (merge x y) ys
    Just LT -> x : y : ys
    Just GT -> y : x : ys
    Just EQ -> y : ys

spanPartialOrd :: Span -> Span -> Maybe Ordering
spanPartialOrd (Span _ xe) (Span ys _) | xe < ys = Just LT
spanPartialOrd (Span xs _) (Span _ ye) | xs > ye = Just GT
spanPartialOrd (Span xs xe) (Span ys ye) | xs == ys && xe == ye = Just EQ
spanPartialOrd _ _ = Nothing

mergeSpan :: Span -> Span -> Span
mergeSpan (Span xs xe) (Span ys ye) = Span (min xs ys) (max xe ye)

insertSpan :: Span -> [Span] -> [Span]
insertSpan = insertPartialOrd spanPartialOrd mergeSpan

mergeSpans' :: [Span] -> [Span] -> [Span]
mergeSpans' = foldl (flip insertSpan)

mergeSpans :: NonEmpty Span -> NonEmpty Span -> NonEmpty Span
mergeSpans s = fromList . mergeSpans' (toList s) . toList

mergeSpannedWith :: (a -> b -> c) -> Spanned a -> Spanned b -> Spanned c
mergeSpannedWith merge (Spanned x xs) (Spanned y ys) = Spanned (x `merge` y) (xs `mergeSpans` ys)

instance Semigroup a => Semigroup (Spanned a) where
  (<>) = mergeSpannedWith (<>)

spanned :: (TraversableStream s, MonadParsec e s m) => m a -> m (Spanned a)
spanned val = (\h v t -> Spanned v (pure $ Span h t)) <$> getSourcePos <*> val <*> getSourcePos

spanAs :: Spanned a -> b -> Spanned b
spanAs i x = Spanned x i.spans
