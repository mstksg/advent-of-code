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

input :: String
input = unsafePerformIO (readFile "input/6.txt")

sites :: [(Int, Int)]
sites = map (\[a, b] -> (read a, read b)) . map (map trim . splitOn ",") . lines $ input

bbox :: (Int, Int, Int, Int)
bbox = (x1, y1, x2, y2)
  where
    x1 = minimum [x | (x, y) <- sites]
    x2 = maximum [x | (x, y) <- sites]
    y1 = minimum [y | (x, y) <- sites]
    y2 = maximum [y | (x, y) <- sites]

type ID = Int

minimums :: Ord b => (a -> b) -> [a] -> [a]
minimums f xs = go xs
  where
    go [] = []
    go (x:xs) = case go xs of
                  (m:ms)
                    | f x == f m -> x:m:ms
                    | f x <  f m -> [x]
                    | otherwise  -> m:ms
                  [] -> [x]


nearest :: (Int, Int) -> Maybe (Int, Int)
nearest (x, y) = case minimums (\(sx, sy) -> abs (x - sx) + abs (y - sy)) sites of
                   [m] -> Just m
                   _ -> Nothing

nearestMap :: Map (Int, Int) [(Int, Int)]
nearestMap = Map.fromListWith (++) [(p,[(x,y)]) | y <- [y1..y2],
                                    x <- [x1..x2],
                                    Just p <- [nearest (x,y)]]
  where
    (x1, y1, x2, y2) = bbox

isEdge :: (Int, Int) -> Bool
isEdge (x, y) = elem x [x1, x2] || elem y [y1, y2]
  where
    (x1, y1, x2, y2) = bbox

solve1 :: Int
solve1 = maximum . fmap length . filter (not . any isEdge) $ toList nearestMap

totalDistance :: (Int,Int) -> Int
totalDistance (x,y) = sum $ map (\(sx, sy) -> abs (x - sx) + abs (y - sy)) sites

within10000 :: [(Int, Int)]
within10000 = [(x,y) |
                y <- [y1..y2],
                x <- [x1..x2],
                totalDistance (x,y) < 10000]
  where
    (x1, y1, x2, y2) = bbox

solve2 :: Int
solve2 = length within10000

main :: IO ()
main = do
  print solve1
  print solve2
