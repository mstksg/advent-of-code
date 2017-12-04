#!/usr/bin/runhaskell

import Control.Monad
import Data.List
import Debug.Trace
import Numeric.Search.Integer

type SquareNumber = Integer
type Index = Integer
type Pos = (Integer, Integer)

squareCorner :: SquareNumber -> Index
squareCorner n
  | n >= 0    = (n * 2 + 1)^2
  | otherwise = 0

squareSize :: SquareNumber -> Integer
squareSize n = (n * 2 + 1)

squareWithIndex :: Index -> SquareNumber
squareWithIndex i = search (\n -> squareCorner n >= i)

cornerPos :: SquareNumber -> Pos
cornerPos n = (n, -n)

u, d, l, r :: Pos -> Pos
u (x, y) = (x, y + 1)
d (x, y) = (x, y - 1)
l (x, y) = (x - 1, y)
r (x, y) = (x + 1, y)

times :: Integer -> (a -> a) -> (a -> a)
times 0 _ = id
times n f = f . (times (n-1) f)

posIndex :: Pos -> Index
posIndex (0, 0) = 1
posIndex (x, y) = squareCorner (square-1) + steps
  where
    square = max (abs x) (abs y)
    size = squareSize square
    steps
      | y == -square = (size - 1) * 3 + (square + x) -- bottom edge
      | x == -square = (size - 1) * 2 + (square - y) -- left edge
      | y == square  = (size - 1) * 1 + (square - x) -- top edge
      | x == square  = (square + y) -- right edge

pos :: Index -> Pos
pos 1 = (0, 0)
pos i = (goU (steps - 1) . r) prevCorner
  where
    prevSquare = squareWithIndex i - 1
    prevCorner = cornerPos (prevSquare)
    size = squareSize (prevSquare + 1)
    steps = i - squareCorner prevSquare

    goU s
      | s > 0     = goL (s - t) . times t u
      | otherwise = id
      where
        t = min s (size - 2)

    goL s
      | s > 0     = goD (s - t) . times t l
      | otherwise = id
      where
        t = min s (size - 1)

    goD s
      | s > 0     = goR (s - t) . times t d
      | otherwise = id
      where
        t = min s (size - 1)

    goR s
      | s > 0     = times t r
      | otherwise = id
      where
        t = min s (size - 1)

mem2 :: Integer -> Integer
mem2 = \i -> ls !! (fromIntegral i - 1)
  where
    ls = map f [1..]
    f 1 = 1
    f i = sum $ do let p = pos i
                   direction <- [u, d, l, r, u.r, u.l, d.l, d.r]
                   let q = direction p
                       qi = posIndex q
                   guard (qi < i)
                   return (mem2 qi)

main :: IO ()
main = do
  print (let (x, y) = pos 277678 in abs x + abs y)
  print (head $ filter (>277678) $ map mem2 [1..])
