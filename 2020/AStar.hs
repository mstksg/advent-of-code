{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}

module AStar where

import           Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set

class AStar s o where
  astarAllOn :: Ord a => (s -> a)
             -> (s -> [s]) -> (s -> s -> Int)
             -> (s -> Int) -> s -> [o]

astarOn :: (Ord a, AStar s o)
        => (s -> a)
        -> (s -> [s]) -> (s -> s -> Int)
        -> (s -> Int) -> s -> o
astarOn f actions cost goal s0 = head (astarAllOn f actions cost goal s0)

astar :: (Ord s, AStar s o)
      => (s -> [s]) -> (s -> s -> Int)
      -> (s -> Int) -> s -> o
astar = astarOn id

astarAll :: (Ord s, AStar s o)
         => (s -> [s]) -> (s -> s -> Int)
         -> (s -> Int) -> s -> [o]
astarAll = astarAllOn id

bfsOn :: (Ord a, AStar s o) => (s -> a) -> (s -> [s]) -> (s -> Bool) -> s -> o
bfsOn f actions goal s0 = astarOn f actions (\_ _ -> 1) (\s -> if goal s then 0 else 1) s0

bfs :: (Ord s, AStar s o) => (s -> [s]) -> (s -> Bool) -> s -> o
bfs actions goal s0 = bfsOn id actions goal s0

dijkstra :: (Ord s, AStar s o) => (s -> [s]) -> (s -> s -> Int) -> s -> [o]
dijkstra actions cost s0 = astarAll actions cost (const 0) s0

exploreOn :: (Ord a, AStar s o) => (s -> a) -> (s -> [s]) -> s -> [o]
exploreOn f actions s0 = astarAllOn f actions (\_ _ -> 1) (const 0) s0

-- Returns the distance to the goal
instance AStar s Int where
  astarAllOn f actions cost goal s0 =
    astarBase f actions cost goal s0 0 (\d s1 s2 -> d + cost s1 s2)

-- Returns the final state
instance AStar s s where
  astarAllOn f actions cost goal s0 =
    astarBase f actions cost goal s0 s0 (\_ s1 s2 -> s2)

-- Returns the final state plus distance
instance AStar s (s, Int) where
  astarAllOn f actions cost goal s0 =
    astarBase f actions cost goal s0 (s0, 0) (\(_,d) s1 s2 -> (s2, d + cost s1 s2))

-- Returns the path to the goal
instance AStar s [s] where
  astarAllOn f actions cost goal s0
    = reverse <$> astarBase f actions cost goal s0 [s0] (\path s1 s2 -> s2 : path)

astarBase :: forall s i a. (Ord a) => (s -> a)
          -> (s -> [s]) -> (s -> s -> Int) -> (s -> Int) -> s
          -> i -> (i -> s -> s -> i) -> [i]
astarBase f actions cost goal s0 i0 iex =
  go Set.empty (H.singleton (goal s0, (s0, 0, i0)))
  where
    go :: Set a -> MinPrioHeap Int (s, Int, i) -> [i]
    go visited frontier =
      case H.view frontier of
        Just ((_, (s, d, i)), frontier')
          | Set.member (f s) visited -> go visited frontier'
          | goal s == 0 ->  i : let new = H.fromList [let d' = d + cost s s'
                                                  in (d' + goal s', (s', d', iex i s s'))
                                                 | s' <- actions s]
                            in go visited' (frontier' <> new)
          | otherwise   ->  let new = H.fromList [let d' = d + cost s s'
                                                  in (d' + goal s', (s', d', iex i s s'))
                                                 | s' <- actions s]
                            in go visited' (frontier' <> new)
            where
              visited' = Set.insert (f s) visited
        Nothing -> []
