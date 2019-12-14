{-# Language DataKinds #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
module Util where

import Data.Char
import Data.Semigroup
import Data.Constraint
import Data.Foldable
import Data.List
import Data.Ord
import GHC.TypeLits

maximumOn :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maximumOn f = maximumBy (comparing f)

magic_addGiven_plusnat :: forall (a :: Nat) (b :: Nat) (c :: Nat).
                          ((a + c) ~ (b + c)) :- (a ~ b)
magic_addGiven_plusnat = Sub Dict

count :: (Foldable f, Eq a) => a -> f a -> Int
count x xs = getSum $ foldMap (\y -> if y == x then 1 else 0) xs

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
