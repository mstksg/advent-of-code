#!/usr/bin/runhaskell
{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Data.Foldable
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U

type Mem = U.Vector Int

-- Returns the first maximum.
maximumOn :: (Foldable f, Ord b) => (a -> b) -> f a -> a
maximumOn f xs = fromMaybe (error "maximumOn") (foldr go Nothing xs)
  where
    go x Nothing = Just x
    go x (Just y)
      | f x >= f y = Just x
      | otherwise  = Just y

redistribute :: Mem -> Mem
redistribute vs = V.generate (V.length vs) newval
  where
    i = maximumOn (vs V.!) [0 .. V.length vs - 1]
    v = vs V.! i
    (q, r) = divMod v (V.length vs)
    redist j = q + (if mod (j - i - 1) (V.length vs) < r then 1 else 0)
    newval j
      | j == i    = redist j
      | otherwise = (vs V.! j) + redist j

text :: String
text = "2 8 8 5 4 2 3 1 5 5 1 2 15 13 5 14"
input = V.fromList $ map read $ words text

stepsToSeen :: Ord a => (a -> a) -> a -> Int
stepsToSeen f x = go 0 Set.empty x
  where
    go !i !seen !x
      | Set.member x seen = i
      | otherwise         = go (i + 1) (Set.insert x seen) (f x)

loopSize :: Ord a => (a -> a) -> a -> Int
loopSize f x = go 0 Map.empty x
  where
    go !i !seen !x = case Map.lookup x seen of
                       Just j -> i - j
                       Nothing -> go (i + 1) (Map.insert x i seen) (f x)

main :: IO ()
main = do
  print (stepsToSeen redistribute input)
  print (loopSize redistribute input)
