-- |
-- Module      : AOC2019.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
module AOC2019.Day12 (
  day12a,
  day12b,
) where

import AOC.Common (clearOut, (!!!))
import AOC.Solver (dyno_, (:~>) (..))
import Data.Char (isDigit)
import Data.List (findIndex)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Sum (..))
import Linear (V3 (..), V4 (..))
import Text.Read (readMaybe)

type Point = V3 Int

data Phase a = Phase {pPos :: !a, pVel :: !a}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

parsePos :: String -> Maybe (Phase Point)
parsePos str = do
  [x, y, z] <- traverse readMaybe . words . clearOut p $ str
  pure $ Phase{pPos = V3 x y z, pVel = 0}
  where
    p '-' = False
    p c = not (isDigit c)

getAccels ::
  Num a =>
  V4 (Phase a) ->
  V4 a
getAccels xs = fmap acc xs
  where
    acc (Phase x _) =
      getSum
        . foldMap (Sum . signum . subtract x . pPos)
        $ xs

step ::
  Num a =>
  V4 (Phase a) ->
  V4 (Phase a)
step ps = update <$> ps <*> getAccels ps
  where
    update (Phase x v) a = Phase (x + v') v'
      where
        v' = v + a

day12a :: V4 (Phase Point) :~> Int
day12a =
  MkSol
    { sParse = \str -> do
        [a, b, c, d] <- traverse parsePos . lines $ str
        pure $ V4 a b c d
    , sShow = show
    , sSolve =
        Just
          . getSum
          . foldMap (Sum . energy)
          . (!!! dyno_ "steps" 1000)
          . iterate step
    }
  where
    energy (Phase x v) = sum (abs x) * sum (abs v)

-- here we run three independent simulations of 4 one-dimensional planets
day12b :: V3 (V4 (Phase Int)) :~> Int
day12b =
  MkSol
    { sParse = \str -> do
        [a, b, c, d] <- traverse parsePos . lines $ str
        pure . traverse sequenceA $ V4 a b c d
    , sShow = show
    , sSolve =
        fmap (foldl1 lcm)
          . traverse (findCycle . NE.tail . NE.iterate step)
          -- find the cycle in each three independent simulations
    }

-- | The cycle is halfway done when the velocities invert (that is, become
-- 0)
findCycle :: (Eq a, Num a) => [V4 (Phase a)] -> Maybe Int
findCycle = fmap ((* 2) . (+ 1)) . findIndex ((== 0) . fmap pVel)
