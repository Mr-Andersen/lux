module Utils where

intersperseM :: Monoid a => a -> [a] -> a
intersperseM sep = go mempty
  where
    go acc [] = acc
    go acc [x] = acc <> x
    go acc (x : xs) = go (acc <> x <> sep) xs
