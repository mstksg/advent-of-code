#!/usr/bin/env stack
-- stack runghc
import           Data.Monoid
import           Data.Foldable
import           Control.Monad
import           System.IO.Unsafe
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map.Strict as Map

input :: String
input = unsafePerformIO (readFile "input/2.txt")

boxids = words input

solve1 = has2 * has3
  where
    counts = map (\w -> Map.fromListWith (+) [(c, 1) | c <- w]) boxids
    has2 = length [cs | cs <- counts, any (==2) cs]
    has3 = length [cs | cs <- counts, any (==3) cs]

solve2 = nub $ do
  a <- boxids
  b <- boxids
  let (same, count) = diff a b
  guard (count == 1)
  return same
  where
    diff a b = fold (zipWith (\c1 c2 -> if c1 /= c2 then (mempty, Sum 1) else ([c1], Sum 0)) a b)


main :: IO ()
main = do
  print solve1
  print solve2
