{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Linear.V2

import           AStar
import           IntCode
import           Util

data Feature = Floor | Wall | Target
  deriving (Show, Eq, Ord)

data S = S (V2 Int) Feature Effect

instance Eq S where
  (S p _ _) == (S q _ _) = p == q

instance Ord S where
  (S p _ _) <= (S q _ _) = p <= q

neighbors :: V2 Int -> [V2 Int]
neighbors p = [p + V2 dx dy | dx <- [-1, 0, 1], dy <- [-1, 0, 1],
               abs dx + abs dy == 1]

dir :: Int -> V2 Int
dir 1 = (V2 0    1)
dir 2 = (V2 0 (-1))
dir 3 = (V2 (-1) 0)
dir 4 = (V2 1    0)

actions :: S -> [S]
actions (S p _ (InputF k)) = do
  d <- [1..4]
  case k d of
    OutputF 0 k -> -- wall
      []
    OutputF 1 k -> -- floor
      return (S (p + dir d) Floor k)
    OutputF 2 k -> -- dest
      return (S (p + dir d) Target k)

goal :: S -> Bool
goal (S p Target k) = True
goal _ = False

prog :: IO IntCode
prog = makeProg . map read . splitOn "," . strip <$> readFile "input/15.txt"

solve1 :: IntCode -> Int
solve1 prog = bfs actions goal (S 0 Floor (step prog))
  where

solve2 :: IntCode -> Int
solve2 prog = maximum (explore o_actions o_start) :: Int
  where
    grid = Map.fromList [(p, f) | S p f _ <- explore actions (S 0 Floor (step prog))]
    o_start = head [p | (p, Target) <- Map.toList grid]
    o_actions p = [q | q <- neighbors p, Map.lookup q grid == Just Floor]
