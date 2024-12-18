-- |
-- Module      : AOC2024.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day18 (
  day18a,
  day18b,
)
where

import AOC.Common.Parser (pDecimal, parseMaybe', sepByLines, sequenceSepBy)
import AOC.Common.Point (Point, cardinalNeighbsSet)
import AOC.Common.Search (bfs, binaryMinSearch)
import AOC.Solver (type (:~>) (..))
import Data.Foldable (Foldable (toList))
import Data.Ix (Ix (inRange))
import Data.List (intercalate)
import Data.Maybe (isNothing)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

solveMaze :: Set Point -> Maybe Int
solveMaze walls = length <$> bfs step 0 (== 70)
  where
    step p = S.filter (all (inRange (0, 70))) $ cardinalNeighbsSet p `S.difference` walls

day18a :: [Point] :~> Int
day18a =
  MkSol
    { sParse = parseMaybe' $ sepByLines $ sequenceSepBy (V2 pDecimal pDecimal) ","
    , sShow = show
    , sSolve = solveMaze . S.fromList . take 1024
    }

day18b :: [Point] :~> Point
day18b =
  MkSol
    { sParse = sParse day18a
    , sShow = intercalate "," . map show . toList
    , sSolve = \pts -> do
        let wallList = Seq.fromList $ scanl (flip S.insert) S.empty pts
        j <- binaryMinSearch (isNothing . solveMaze . Seq.index wallList) 0 (length pts)
        pure $ pts !! (j - 1)
    }
