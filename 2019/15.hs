{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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

data S = S { pos :: (V2 Int),
             feature :: Feature,
             vmstate :: Effect }

neighbors :: V2 Int -> [V2 Int]
neighbors p = [p + V2 dx dy | dx <- [-1, 0, 1], dy <- [-1, 0, 1],
               abs dx + abs dy == 1]

dir :: Int -> V2 Int
dir 1 = (V2 0    1)
dir 2 = (V2 0 (-1))
dir 3 = (V2 (-1) 0)
dir 4 = (V2 1    0)

actions :: S -> [S]
actions (S p f (InputF k)) = do
  guard (f /= Wall)
  d <- [1..4]
  case k d of
    OutputF 0 k -> -- wall
      return (S (p + dir d) Wall k)
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
solve1 prog = bfsOn pos actions goal (S 0 Floor (step prog))

solve2 :: IntCode -> Int
solve2 prog = maximum (exploreOn id o_actions o_start) :: Int
  where
    grid = Map.fromList [(p, f) | S p f _ <- exploreOn pos actions (S 0 Floor (step prog))]
    o_start = head [p | (p, Target) <- Map.toList grid]
    o_actions p = [q | q <- neighbors p, Map.lookup q grid == Just Floor]

showmap :: IntCode -> IO ()
showmap prog = T.putStrLn $ T.unlines $ do
  y <- reverse [ymin..ymax]
  return $ fold $ do
    x <- [xmin..xmax]
    return $ case Map.lookup (V2 x y) grid of
      Just Floor
        | (V2 x y == 0) -> red "x"
        | Set.member (V2 x y) path -> red "."
        | otherwise -> "."
      Just Wall -> "#"
      Just Target -> red "o"
      Nothing -> "?"
  where
    grid = Map.fromList [(p, f) | S p f _ <- exploreOn pos actions (S 0 Floor (step prog))]
    path = Set.fromList [p | S p f k <- bfsOn pos actions goal (S 0 Floor (step prog))]
    (V2 xmin ymin, V2 xmax ymax) = bbox (Map.keysSet grid)

bbox :: Set (V2 Int) -> (V2 Int, V2 Int)
bbox pts = (foldr1 (liftA2 min) pts,
            foldr1 (liftA2 max) pts)
