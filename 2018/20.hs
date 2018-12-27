#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Heap (MinHeap)
import qualified Data.Heap as H
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ord
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util.Util

data Pos = Pos {
  x :: Int,
  y :: Int
  }
  deriving (Show, Eq, Ord)

instance Num Pos where
  fromInteger n = Pos (fromInteger n) (fromInteger n)
  Pos x1 y1 + Pos x2 y2 = Pos (x1 + x2) (y1 + y2)
  Pos x1 y1 - Pos x2 y2 = Pos (x1 - x2) (y1 - y2)
  Pos x1 y1 * Pos x2 y2 = Pos (x1 * x2) (y1 * y2)
  abs _ = undefined
  signum _ = undefined

data Expr = Step Char | Branch [Expr] | Sequence [Expr]
  deriving (Show, Eq, Ord)

pInput :: CharParsing m => m Expr
pInput = between (char '^') (char '$') pSeq
  where
    pSeq = Sequence <$> many pExpr <* spaces
    pExpr = (Step <$> oneOf "NSEW") <|>
            (Branch <$> between (char '(') (char ')') (sepBy pSeq (char '|')))

door :: Pos -> Pos -> Set (Pos,Pos)
door x y = Set.fromList [(x,y),(y,x)]

translate :: Pos -> (Set (Pos,Pos), Set Pos) -> (Set (Pos,Pos), Set Pos)
translate p (door, out) = (door', out')
  where
    door' = Set.fromList [(d1 + p, d2 + p) | (d1,d2) <- Set.toList door]
    out' = Set.fromList [o + p | o <- Set.toList out]

runExpr :: Expr -> (Set (Pos,Pos), Set Pos)
runExpr (Step 'E') = (door 0 (Pos 1 0), Set.fromList [Pos 1 0])
runExpr (Step 'W') = (door 0 (Pos (-1) 0), Set.fromList [Pos (-1) 0])
runExpr (Step 'S') = (door 0 (Pos 0 1), Set.fromList [Pos 0 1])
runExpr (Step 'N') = (door 0 (Pos 0 (-1)), Set.fromList [Pos 0 (-1)])
runExpr (Sequence xs) = foldr seq (Set.empty, Set.singleton 0) xs
  where
    seq e next = case runExpr e of
      (door1, out1) -> (Set.union door1 (fold [fst (translate p next) | p <- toList out1]),
                        fold [snd (translate p next) | p <- toList out1])
runExpr (Branch xs) = foldMap runExpr xs

data Rect = Rect Pos Pos
  deriving (Show, Eq, Ord)

bbox :: [Pos] -> Rect
bbox [] = Rect 0 0
bbox xs = Rect (Pos xmin ymin) (Pos xmax ymax)
  where
    xmin = minimum (map x xs)
    ymin = minimum (map y xs)
    xmax = maximum (map x xs) + 1
    ymax = maximum (map y xs) + 1

drawMap :: Set (Pos,Pos) -> String
drawMap doors = fold [line y | y <- [y1..y2-1]]
  where
    line y = unlines $ map fold $ transpose [draw (Pos x y) | x <- [x1..x2-1]]
    draw p = ["#" ++ u ++ "#", l ++ "  " ++ r, l ++ "  " ++ r, "#" ++ d ++ "#"]
      where
        u = if Set.member (p, p + Pos 0 (-1)) doors then "--" else "##"
        l = if Set.member (p, p + Pos (-1) 0) doors then "|" else "#"
        r = if Set.member (p, p + Pos 1 0) doors then "|" else "#"
        d = if Set.member (p, p + Pos 0 1) doors then "--" else "##"

    Rect (Pos x1 y1) (Pos x2 y2) = bbox $ foldMap (toListOf each) $ doors

-- Returns all reachable goals by cost
dijkstra :: forall s. (Ord s) => (s -> [s]) -> (s -> s -> Int) -> s -> [(s, Int)]
dijkstra actions cost s0 = go Set.empty (H.singleton (0, s0))
  where
    go :: Set s -> MinHeap (Int, s) -> [(s, Int)]
    go visited frontier =
      case H.view frontier of
        Just ((d, s), frontier')
          | Set.member s visited -> go visited frontier'
          | otherwise   ->  let new = H.fromList [let d' = d + cost s s'
                                                  in (d', s')
                                                 | s' <- actions s]
                            in (s,d) : go visited' (frontier' <> new)
            where
              visited' = Set.insert s visited
        Nothing -> []

solve1 :: Set (Pos, Pos) -> Int
solve1 doors = maximum $ map snd $ dijkstra next (\_ _ -> 1) 0
  where
    next p = Map.findWithDefault [] p doorMap
    doorMap = Map.fromListWith (<>) [(p1, [p2]) | (p1, p2) <- Set.toList doors]

count p xs = length [x | x <- xs, p x]

solve2 :: Set (Pos, Pos) -> Int
solve2 doors = count ((>=1000) . snd) $ dijkstra next (\_ _ -> 1) 0
  where
    next p = Map.findWithDefault [] p doorMap
    doorMap = Map.fromListWith (<>) [(p1, [p2]) | (p1, p2) <- Set.toList doors]

main :: IO ()
main = do
  txt <- readFile "input/20.txt"
  let input = parse pInput txt
      doors = fst $ runExpr input
  putStrLn (drawMap $ doors)
  print (solve1 doors)
  print (solve2 doors)
