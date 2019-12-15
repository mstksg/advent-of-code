{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}

module AStar where

import           Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set

class Ord s => AStar s o where
  astar :: (s -> [s]) -> (s -> s -> Int) -> (s -> Int) -> s -> o
  astarAll :: (s -> [s]) -> (s -> s -> Int) -> (s -> Int) -> s -> [o]

bfs :: AStar s o => (s -> [s]) -> (s -> Bool) -> s -> o
bfs actions goal s0 = astar actions (\_ _ -> 1) (\s -> if goal s then 0 else 1) s0

dijkstra :: AStar s o => (s -> [s]) -> (s -> s -> Int) -> s -> [o]
dijkstra actions cost s0 = astarAll actions cost (const 0) s0

-- Returns the distance to the goal
instance Ord s => AStar s Int where
  astar actions cost goal s0 = head (astarAll actions cost goal s0)
  astarAll actions cost goal s0 = astarBase actions cost goal s0 0 (\d s1 s2 -> d + cost s1 s2)

-- Returns the final state
instance Ord s => AStar s s where
  astar actions cost goal s0 = head (astarAll actions cost goal s0)
  astarAll actions cost goal s0 = astarBase actions cost goal s0 s0 (\_ s1 s2 -> s2)

-- Returns the path to the goal
instance Ord s => AStar s [s] where
  astar actions cost goal s0 = head (astarAll actions cost goal s0)
  astarAll actions cost goal s0 = reverse <$> astarBase actions cost goal s0 [s0] (\path s1 s2 -> s2 : path)

explore :: AStar s o => (s -> [s]) -> s -> [o]
explore actions s0 = astarAll actions (\_ _ -> 1) (const 0) s0

astarBase :: forall s i. (Ord s)
          => (s -> [s]) -> (s -> s -> Int) -> (s -> Int) -> s
          -> i -> (i -> s -> s -> i) -> [i]
astarBase actions cost goal s0 i0 iex = go Set.empty (H.singleton (goal s0, (s0, 0, i0)))
  where
    go :: Set s -> MinPrioHeap Int (s, Int, i) -> [i]
    go visited frontier =
      case H.view frontier of
        Just ((_, (s, d, i)), frontier')
          | Set.member s visited -> go visited frontier'
          | goal s == 0 ->  i : let new = H.fromList [let d' = d + cost s s'
                                                  in (d' + goal s', (s', d', iex i s s'))
                                                 | s' <- actions s]
                            in go visited' (frontier' <> new)
          | otherwise   ->  let new = H.fromList [let d' = d + cost s s'
                                                  in (d' + goal s', (s', d', iex i s s'))
                                                 | s' <- actions s]
                            in go visited' (frontier' <> new)
            where
              visited' = Set.insert s visited
        Nothing -> []
