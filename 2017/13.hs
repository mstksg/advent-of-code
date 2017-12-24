#!/usr/bin/runhaskell
{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.Graph.Inductive.Query.DFS
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Semigroup
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

data Layer = Layer Int Int
  deriving (Show, Eq, Ord)

parse :: Parsec.Parsec String () c -> String -> c
parse p t = either (error . show) id (Parsec.parse p "" t)

parseLayer :: (Monad m, CharParsing m) => m Layer
parseLayer = do a <- read <$> many digit
                string ": "
                b <- read <$> many digit
                return (Layer a b)

parseInput :: (Monad m, CharParsing m) => m [Layer]
parseInput = sepBy parseLayer (char '\n')

caught :: Int -> Layer -> Bool
caught t (Layer d r) = mod (t + d) ((r-1)*2) == 0

severity :: Int -> Layer -> Int
severity _ (Layer d 1) = d
severity t (Layer d r) = case mod (t + d) ((r-1)*2) of
                           0 -> d * r
                           _ -> 0

main :: IO ()
main = do
  text <- readFile "input/13"
  let gates = parse parseInput text
  print $ foldMap (Sum . severity 0) gates
  print $ head [t | t <- [0..], not (any (caught t) gates)]
  let t = head [t | t <- [0..], not (any (caught t) gates)]
  print $ map (severity t) gates
