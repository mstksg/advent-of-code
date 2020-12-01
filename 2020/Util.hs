module Util where

import Control.Monad
import Data.Foldable

the :: (Foldable f, Eq a) => f a -> a
the = theL . toList
  where
    theL [] = error "empty list"
    theL (x:xs) | all (==x) xs = x
    theL xs = error "not unique"
