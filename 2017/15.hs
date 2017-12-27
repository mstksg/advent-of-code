#!/usr/bin/runhaskell

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Bits.Lens
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.Graph.Inductive.Query.DFS
import           Data.Int
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List
import           Data.List.Split
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Semigroup
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Numeric
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

gen :: Int64 -> (Int64 -> Int64)
gen factor x = mod (factor * x) 2147483647

genA, genB :: Int64 -> Int64
genA = gen 16807
genB = gen 48271

main :: IO ()
main = do
  let
    seqA = tail (iterate genA 883)
    seqB = tail (iterate genB 879)
  print $ sum [if mod a 0x10000 == mod b 0x10000 then 1 else 0 | (a, b) <- take (40 * 10^6) (zip seqA seqB)]

  let
    seqA' = filter (\x -> mod x 4 == 0) seqA
    seqB' = filter (\x -> mod x 8 == 0) seqB
  print $ sum [if mod a 0x10000 == mod b 0x10000 then 1 else 0 | (a, b) <- take (5 * 10^6) (zip seqA' seqB')]
