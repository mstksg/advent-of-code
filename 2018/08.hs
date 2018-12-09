#!/usr/bin/env stack
-- stack runghc

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           System.IO.Unsafe
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

trim = dropWhile isSpace . dropWhileEnd isSpace

parse :: Parsec.Stream s Identity t => Parsec.Parsec s () c -> s -> c
parse p s = either (error.show) id (Parsec.parse p "" s)

input :: String
input = unsafePerformIO (readFile "input/8.txt")

data Tree = Tree [Tree] [Int]
  deriving (Show, Eq, Ord)

tree :: Tree
tree = parse ptree input
  where
    num = read <$> some digit <* spaces
    ptree = do
      numChildren <- num
      numData <- num
      children <- replicateM numChildren ptree
      metadata <- replicateM numData num
      return (Tree children metadata)

sumMetadata :: Tree -> Int
sumMetadata (Tree children meta) = sum meta + sum (map sumMetadata children)

value :: Tree -> Int
value (Tree children meta)
  | null children = sum meta
  | otherwise = sum [value (children !! (i-1)) | i <- meta, 1 <= i && i <= n]
  where
    n = length children

solve1 = sumMetadata tree

solve2 = value tree
