#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE FlexibleContexts #-}

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
input = unsafePerformIO (readFile "input/7.txt")

steps :: Set Char
steps = Map.keysSet deps <> fold deps

deps :: Map Char (Set Char)
deps = Map.fromListWith (<>) [(t, Set.singleton d) | (t, d) <- bits]
  where
    bits = parse (endBy entry spaces) input
    entry = do string "Step "
               d <- anyChar
               string " must be finished before step "
               t <- anyChar
               string " can begin."
               return (t, d)

depsOf :: Char -> Set Char
depsOf c = Map.findWithDefault Set.empty c deps

solve1 :: [Char]
solve1 = go Set.empty steps
  where
    go done next
      | Set.null next = []
      | otherwise = case minimum [(Set.size (Set.difference (depsOf c) done), c) | c <- toList next] of
                      (0, c) -> c : go (Set.insert c done) (Set.delete c next)

time c = 60 + (ord c - ord 'A' + 1)

solve2 :: Int
solve2 = sum $ go Set.empty [] steps
  where
    go done workers next
      | Set.null next = [maximum [t | (c, t) <- workers]]
      | length workers == 5 = wait done workers next
      | otherwise = case sort [c | c <- toList next, Set.isSubsetOf (depsOf c) done] of
                      [] -> wait done workers next
                      (c:cs) -> go done (workers ++ [(c, time c)]) (Set.delete c next)
    wait done workers next = t1 : go (Set.fromList cfinished <> done) workers' next
      where
        t1 = minimum [t | (c, t) <- workers]
        cfinished = [c | (c, t) <- workers, t == t1]
        workers' = [(c, t - t1) | (c, t) <- workers, t > t1]

main :: IO ()
main = do
  putStrLn solve1
  print solve2
