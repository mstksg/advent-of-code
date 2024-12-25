-- |
-- Module      : AOC2024.Day25
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day25 (
  day25a,
)
where

import AOC.Common (intFreqs, lookupIntFreq)
import AOC.Common.Point (Point, parseAsciiSet)
import AOC.Solver (noFail, type (:~>) (..))
import Control.Lens (view)
import Control.Monad (guard)
import Data.Foldable (Foldable (toList))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Set (Set)
import Linear.V2 (R1 (_x), R2 (_y))

day25a :: [Set Point] :~> Int
day25a =
  MkSol
    { sParse = noFail $ map (parseAsciiSet (== '#')) . splitOn "\n\n"
    , sShow = show
    , sSolve =
        noFail $
          uncurry countCombos . partition isLock
    }
  where
    isLock = (== 5) . lookupIntFreq 0 . intFreqs . map (view _y) . toList
    countCombos locks keys = length do
      lock <- colCounts <$> locks
      key <- colCounts <$> keys
      guard . all (< 8) $ IM.unionWith (+) lock key

colCounts :: Set Point -> IntMap Int
colCounts = intFreqs . map (view _x) . toList
