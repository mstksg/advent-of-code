#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
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
import           Util.Util

newtype Id = Id Int
  deriving (Show, Eq, Ord)

type Grid a = Vector (Vector a)

data Pos = Pos {
  x :: Int,
  y :: Int
  }
  deriving (Show, Eq)

indexGrid :: Grid a -> Pos -> a
indexGrid grid (Pos x y) = grid V.! y V.! x

norm1 :: Pos -> Int
norm1 (Pos x y) = abs x + abs y

-- Use reading (raster scan) order for positions.
instance Ord Pos where
  compare (Pos x y) (Pos a b)
    = compare y b <> compare x a

instance Num Pos where
  fromInteger n = Pos (fromInteger n) (fromInteger n)
  Pos x1 y1 + Pos x2 y2 = Pos (x1 + x2) (y1 + y2)
  Pos x1 y1 - Pos x2 y2 = Pos (x1 - x2) (y1 - y2)
  Pos x1 y1 * Pos x2 y2 = Pos (x1 * x2) (y1 * y2)
  abs _ = undefined
  signum _ = undefined

neighbors :: Pos -> [Pos]
neighbors p = [p + d | d <- [Pos 0 1, Pos 0 (-1), Pos 1 0, Pos (-1) 0]]

data Allegiance = G | E
  deriving (Show, Read, Eq, Ord)

data Unit = Unit {
  unitId :: Id,
  unitPos :: Pos,
  unitHP :: Int,
  unitType :: Allegiance,
  unitPower :: Int
  }
  deriving (Show, Eq, Ord)

data Battlefield = Battlefield {
  bFloor :: Grid Bool,
  bUnits :: Map Id Unit,
  bUnitsMap :: Map Pos Id
  }
  deriving (Show, Eq, Ord)

loadBattlefield :: String -> Battlefield
loadBattlefield txt = Battlefield floor unitsById unitsByPos
  where
    ls = lines txt
    units = [(read [c] :: Allegiance, Pos x y) |
              (y, line) <- zip [0..] (lines txt),
              (x, c) <- zip [0..] line,
              elem c ['G', 'E']]
    unitsById = Map.fromList [(i, Unit i pos 200 t 3) | (i, (t, pos)) <- zip (map Id [1..]) units]
    unitsByPos = Map.fromList [(unitPos u, unitId u) | u <- Map.elems unitsById]
    floor = V.fromList [V.fromList [elem c ['.', 'E', 'G'] | c <- line] | line <- lines txt]

-- === pathing === --

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f xs = groupBy (\x y -> f x == f y) xs

-- Returns all reachable goals by cost
astar :: forall s. (Ord s) => (s -> [s]) -> (s -> s -> Int) -> (s -> Int) -> s -> [(s, Int)]
astar actions cost goal s0 = go Set.empty (H.singleton (goal s0, (s0, 0)))
  where
    go :: Set s -> MinHeap (Int, (s, Int)) -> [(s, Int)]
    go visited frontier =
      case H.view frontier of
        Just ((_, (s, d)), frontier')
          | Set.member s visited -> go visited frontier'
          | goal s == 0 ->  (s,d) : go visited' frontier'
          | otherwise   ->  let new = H.fromList [let d' = d + cost s s'
                                                  in (d' + goal s', (s', d'))
                                                 | s' <- actions s]
                            in go visited' (frontier' <> new)
            where
              visited' = Set.insert s visited
        Nothing -> []

-- Returns all the nearest goals.
-- astar' :: Ord s => (s -> [s]) -> (s -> s -> Int) -> (s -> Int) -> s -> [s]
astar' actions cost goal s0 = case astar actions cost goal s0 of
                                (p1, dist) : xs -> p1 : map fst (takeWhile (\(p,d) -> d == dist) xs)
                                [] -> []


-- === Unit Turn === --

killUnit :: Unit -> Battlefield -> Battlefield
killUnit u grid = grid { bUnits = Map.delete (unitId u) (bUnits grid),
                         bUnitsMap = Map.delete (unitPos u) (bUnitsMap grid) }

updateUnit :: Unit -> Battlefield -> Battlefield
updateUnit u grid = grid { bUnits = newUnits, bUnitsMap = newMap }
  where
    oldPos = unitPos (bUnits grid Map.! unitId u)
    newUnits = Map.insert (unitId u) u $ bUnits grid
    newMap = if unitPos u /= oldPos then
               Map.insert (unitPos u) (unitId u) $ Map.delete oldPos $ bUnitsMap grid
             else bUnitsMap grid

moveUnit :: Unit -> Pos -> Battlefield -> Battlefield
moveUnit u p grid = updateUnit (u { unitPos = p }) grid

damageUnit :: Unit -> Int -> Battlefield -> Battlefield
damageUnit u dmg grid
      | newHP <= 0 = killUnit u grid
      | otherwise  = updateUnit (u{ unitHP = newHP}) grid
      where
        newHP = unitHP u - dmg

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f xs = minimumBy (comparing f) xs

tryMove :: Id -> Battlefield -> Battlefield
tryMove ix grid
  | null targets = grid -- No targets, battle is over.
  | any (isAdjacent u) targets = grid -- Already adjacent.
  | null targetSquares = grid
  | otherwise    = move
  where
    u = bUnits grid Map.! ix
    targets = [bUnits grid Map.! j
              | (j, ju) <- Map.toList (bUnits grid),
                unitType ju /= unitType u]
    targetSquares = Set.fromList [p | ju <- targets, p <- neighbors (unitPos ju), isFree p]
    isFree p = indexGrid (bFloor grid) p && Map.notMember p (bUnitsMap grid)
    isAdjacent u ju = norm1 (unitPos ju - unitPos u) == 1

    move = let actions p = [p' | p' <- neighbors p, isFree p']
               cost _ _ = 1
               goal p = if Set.member p targetSquares then 0 else 1
           in case astar' actions cost goal (unitPos u) of
                [] -> grid -- Can't reach anyone.
                positions -> let tpos = minimum positions -- First in reading order
                             in move' tpos

    move' tpos = let actions p = [p' | p' <- neighbors p, isFree p']
                     cost _ _ = 1
                     goal p = if norm1 (p - unitPos u) == 1 then 0 else 1
                 in case astar' actions cost goal tpos of
                      [] -> error "unreachable"
                      nextSteps -> moveUnit u (minimum nextSteps) grid -- First in reading order

tryAttack :: Id -> Battlefield -> Battlefield
tryAttack ix grid
  | any (isAdjacent u) targets = -- Adjacent to target, attack the first.
      attack (minimumOn (unitHP &&& unitPos) [ju | ju <- targets, isAdjacent u ju]) grid
  | otherwise    = grid
  where
    u = bUnits grid Map.! ix
    targets = [bUnits grid Map.! j
              | (j, ju) <- Map.toList (bUnits grid),
                unitType ju /= unitType u]
    isAdjacent u ju = norm1 (unitPos ju - unitPos u) == 1

    attack tgt grid = damageUnit tgt (unitPower u) grid


takeTurn :: Id -> Battlefield -> Battlefield
takeTurn ix grid = tryAttack ix (tryMove ix grid)

combatOver :: Map Id Unit -> Bool
combatOver units = Set.size (foldMap (Set.singleton . unitType) units) < 2

simulate :: Battlefield -> (Bool, Battlefield)
simulate grid = go turns grid
  where
    turns = Map.elems (bUnitsMap grid)
    go [] grid = (False, grid)
    go (ix:xs) grid = if Map.member ix (bUnits grid) then
                        if combatOver (bUnits grid) then
                          (True, grid)
                        else
                          go xs (takeTurn ix grid)
                      else
                        go xs grid

display :: Battlefield -> String
display grid = unlines [[drawCell (Pos x y) | (x, f) <- zip [0..] (V.toList line)]
                       | (y, line) <- zip [0..] (V.toList (bFloor grid))]
  where
    drawCell p = case Map.lookup p (bUnitsMap grid) of
      Just ix -> case unitType (bUnits grid Map.! ix) of
                   E -> 'E'
                   G -> 'G'
      Nothing -> case indexGrid (bFloor grid) p of
                   True -> '.'
                   False -> '#'

solve1 :: Battlefield -> Int
solve1 grid = go 0 grid
  where
    go i grid = case simulate grid of
      (True, grid) -> i * sum (map unitHP $ Map.elems (bUnits grid))
      (False, grid) -> go (i+1) grid

elfPower :: Int -> Battlefield -> Battlefield
elfPower x grid = grid { bUnits = newUnits }
  where
    newUnits = (\u -> if unitType u == E then
                        u { unitPower = x }
                      else u) <$> (bUnits grid)

totalCasualties :: Allegiance -> Battlefield -> Battlefield -> Int
totalCasualties a start end = length [u | u <- Map.elems (bUnits start), unitType u == a]
                              - length [u | u <- Map.elems (bUnits end), unitType u == a]

runCompletely :: Battlefield -> (Int, Battlefield)
runCompletely grid = go 0 grid
  where
    go i grid = case simulate grid of
      (True, grid) -> (i * sum (map unitHP $ Map.elems (bUnits grid)), grid)
      (False, grid) -> go (i+1) grid

solve2 :: Battlefield -> Int
solve2 grid = head [score | p <- [3..],
                     let (score, end) = runCompletely (elfPower p grid),
                     totalCasualties E grid end == 0]


main :: IO ()
main = do
  txt <- readFile "input/15.txt"
  let g = loadBattlefield txt
  print $ solve1 g
  print $ solve2 g
  let
    go grid = do
        putStr (display grid)
        threadDelay (100 * 10^3)
        go (snd $ simulate $ elfPower 19 grid)
  go g
