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

parse :: Parsec.Parsec String () c -> String -> c
parse p t = either (error . show) id (Parsec.parse p "" t)

parseDoor :: (Monad m, CharParsing m) => m (Int, [Int])
parseDoor = do a <- read <$> many digit
               string " <-> "
               bs <- map read <$> sepBy (many digit) (string ", ")
               return (a, bs)

-- parseInput :: (Monad m, CharParsing m) => m [(Int, [Int])]
-- parseInput = sepBy parseDoor (char '\n')

parseInput :: (Monad m, CharParsing m) => m (Gr () ())
parseInput = do links <- sepBy parseDoor (char '\n')
                return $ mkGraph [(n, ()) | (n, _) <- links] [(n, m, ()) | (n, ms) <- links, m <- ms]

main :: IO ()
main = do
  text <- readFile "input/11"
  let gr = parse parseInput text
  print $ length $ dfs [0] gr
  print $ length $ components gr
