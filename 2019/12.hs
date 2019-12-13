{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Lens
import           Data.Foldable
import           Control.Applicative
import           Debug.Trace
import           Control.Monad
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Linear.V2
import           Linear.V3
import           Linear.V4


data Moons = Moons {
  pos :: !(V4 Int),
  vel :: !(V4 Int)
  }
  deriving (Show, Eq, Ord)

step :: Moons -> Moons
step Moons{..} = Moons{ pos = pos', vel = vel'}
  where
    !acc = (\p -> sum [signum (a - p) | a <- toList pos]) <$> pos
    !vel' = vel + acc
    !pos' = pos + vel'

input :: V3 Moons
input = (V3
         (Moons (V4 (-9) 2 10 (-6)) 0)
         (Moons (V4 (-1) 9 18 15) 0)
         (Moons (V4 (-1) 5 (-12) (-7)) 0))

-- sample :: Moons
-- sample = Moons (V4 (V3 (-1) 0 2)
--                 (V3 2 (-10) (-7))
--                  (V3 4 (-8) 8)
--                  (V3 3 5 (-1))) 0

energy :: V3 Moons -> V4 Int
energy xyz = liftA2 (\p v -> sum (abs <$> p) * sum (abs <$> v)) pos' vel'
  where
    pos' = sequence (pos <$> xyz) :: V4 (V3 Int)
    vel' = sequence (vel <$> xyz) :: V4 (V3 Int)

solve1 :: V3 Moons -> Int
solve1 start = sum $ energy (iterate (fmap step) start !! 1000)

solve2 :: V3 Moons -> Int
solve2 start = foldr1 lcm $ (\axis -> go axis 0 Map.empty) <$> start
  where
    go !m !i !seen = case Map.lookup m seen of
      Just k -> i
      Nothing -> go (step m) (i+1) (Map.insert m i seen)
