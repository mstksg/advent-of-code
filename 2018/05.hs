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
import           System.IO.Unsafe
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

input :: String
input = filter (not . isSpace) $ unsafePerformIO (readFile "input/5.txt")

invert :: Char -> Char
invert c
  | isLower c = toUpper c
  | isUpper c = toLower c
  | otherwise = c

react :: String -> String
react = go []
  where
    go cs [] = reverse cs
    go [] (x:xs) = go [x] xs
    go (c:cs) (x:xs)
      | invert c == x = go cs xs
      | otherwise     = go (x:c:cs) xs

solve1 :: Int
solve1 = length (react input)

solve2 :: Int
solve2 = minimum [removed c | c <- nub $ map toUpper input]
  where
    removed c = length $ react $ filter (\x -> not (elem x [c, toLower c])) input
