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

import           Knot

showBinary :: [Word8] -> String
showBinary xs = map b $ foldMap (\w -> reverse $ w ^.. bits) xs
  where
    b True = '1'
    b False = '0'

nodeId :: Int -> Int -> Int
nodeId x y = y * 128 + x

nodePos :: Int -> (Int, Int)
nodePos n = (n `mod` 128, n `div` 128)

main :: IO ()
main = do
  let
    grid :: Vector (U.Vector Int32)
    grid = V.fromList [U.fromList [if b == '1' then 1 else 0 | b <- showBinary (hashData key)] | r <- [0..127], let key = "oundnydw-" ++ show r]
  print $ foldMap (Sum . V.sum) grid
  let
    nodes = do
      y <- [0..127]
      x <- [0..127]
      guard (grid V.! y V.! x == 1)
      return (nodeId x y, (x, y))
    edges = do
      y <- [0..127]
      x <- [0..127]
      (x2, y2) <- [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]
      guard (0 <= x2 && x2 <= 127)
      guard (0 <= y2 && y2 <= 127)
      guard (grid V.! y V.! x == 1 && grid V.! y2 V.! x2 == 1)
      return (nodeId x y, nodeId x2 y2, ())
    graph :: Gr (Int, Int) ()
    graph = mkGraph nodes edges
  print (noComponents graph)
