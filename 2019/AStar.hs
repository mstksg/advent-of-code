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

instance Ord s => AStar s Int where
  astar actions cost goal s0 = fromJust (astar actions cost goal s0)

instance Ord s => AStar s (Maybe Int) where
  astar actions cost goal s0 = listToMaybe $ astarBase actions cost goal s0 0 (\d s1 s2 -> d + cost s1 s2)

instance Ord s => AStar s [s] where
  astar actions cost goal s0 = fromJust (astar actions cost goal s0)

instance Ord s => AStar s (Maybe [s]) where
  astar actions cost goal s0 = listToMaybe $ reverse <$> astarBase actions cost goal s0 [s0] (\path s1 s2 -> s2 : path)

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
          | goal s == 0 ->  i : go visited' frontier'
          | otherwise   ->  let new = H.fromList [let d' = d + cost s s'
                                                  in (d' + goal s', (s', d', iex i s s'))
                                                 | s' <- actions s]
                            in go visited' (frontier' <> new)
            where
              visited' = Set.insert s visited
        Nothing -> error "no route"
