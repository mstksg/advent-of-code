{-# Language DataKinds #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
module Util where

import Data.Constraint
import GHC.TypeLits
import Data.Ord
import Data.Foldable

maximumOn :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maximumOn f = maximumBy (comparing f)

magic_addGiven_plusnat :: forall (a :: Nat) (b :: Nat) (c :: Nat).
                          ((a + c) ~ (b + c)) :- (a ~ b)
magic_addGiven_plusnat = Sub Dict
