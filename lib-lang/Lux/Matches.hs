{-# LANGUAGE UndecidableInstances #-}

module Lux.Matches where

import Data.Functor.Identity (Identity)
import Lux.UnSpan
import Text.Megaparsec.Spanned (Spanned)

class Matches f where
  (=~) :: f Spanned -> f Identity -> Bool

instance (UnSpan f, Eq (f Identity)) => Matches f where
  s =~ i = unSpan s == i
