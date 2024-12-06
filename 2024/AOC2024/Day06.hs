-- |
-- Module      : AOC2024.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day06 (
  day06a,
  day06b,
)
where

import AOC.Common (countTrue, findLoop_, iterateMaybe)
import AOC.Common.Point (
  Dir (..),
  Point,
  V2 (..),
  addAxesMap,
  boundingBox,
  collapseAxes,
  dirPoint,
  inBoundingBox,
  parseAsciiMap,
 )
import AOC.Solver (noFail, type (:~>) (..))
import Control.Monad (guard)
import Data.Functor (($>), (<&>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES

parseMap :: String -> Maybe (NESet Point, Point)
parseMap str = (,) <$> NES.nonEmptySet (M.keysSet boulders) <*> S.lookupMin (M.keysSet startPos)
  where
    (boulders, startPos) = M.partition id . flip parseAsciiMap str $ \case
      '#' -> Just True
      '^' -> Just False
      _ -> Nothing

-- | Could be infinite
stepPath :: V2 Point -> NESet Point -> Point -> [(Point, Dir)]
stepPath bb boulders = iterateMaybe go . (,South)
  where
    go (x, d) =
      guard (inBoundingBox bb x)
        $> if x' `NES.member` boulders
          then (x, d <> West)
          else (x', d)
      where
        x' :: Point
        x' = x + dirPoint d

day06a :: (NESet Point, Point) :~> _
day06a =
  MkSol
    { sParse = parseMap
    , sShow = show
    , sSolve =
        noFail \(boulders, startPos) ->
          subtract 1 . S.size . S.fromList $
            fst <$> stepPath (boundingBox boulders) boulders startPos
    }

-- | Could be infinite
stepPath' :: V2 (Map Int (Set Int)) -> Point -> [(Point, Dir)]
stepPath' (V2 xMap yMap) = iterateMaybe go . (,South)
  where
    go (V2 x y, d) = case d of
      North -> S.lookupGT y (M.findWithDefault mempty x xMap) <&> \y' -> (V2 x (y' - 1), West)
      East -> S.lookupGT x (M.findWithDefault mempty y yMap) <&> \x' -> (V2 (x' - 1) y, North)
      South -> S.lookupLT y (M.findWithDefault mempty x xMap) <&> \y' -> (V2 x (y' + 1), East)
      West -> S.lookupLT x (M.findWithDefault mempty y yMap) <&> \x' -> (V2 (x' + 1) y, South)

day06b :: (NESet Point, Point) :~> Int
day06b =
  MkSol
    { sParse = sParse day06a
    , sShow = show
    , sSolve =
        noFail \(boulders, startPos) ->
          let bb = boundingBox boulders
              origPath = S.fromList $ fst <$> stepPath bb boulders startPos
              axesMaps = collapseAxes boulders
           in flip countTrue origPath \p ->
                p /= startPos && findLoop_ (stepPath' (addAxesMap p axesMaps) startPos)
    }
