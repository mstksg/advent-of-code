-- |
-- Module      : AOC2024.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day12 (
  day12a,
  day12b,
)
where

import AOC.Common.Point (Dir (North), Point, contiguousRegions, dirPoint, parseAsciiMap)
import AOC.Solver (noFail, type (:~>) (..))
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES

regionNeighbors :: Set Point -> Map Dir (Set Point)
regionNeighbors pts =
  M.fromListWith
    (<>)
    [ (d, S.singleton p')
    | p <- toList pts
    , d <- [North ..]
    , let p' = p + dirPoint d
    , p' `S.notMember` pts
    ]

regions :: Ord a => Map Point a -> Map a (Set (NESet Point))
regions mp =
  contiguousRegions
    <$> M.fromListWith
      (<>)
      [ (x, S.singleton p)
      | (p, x) <- M.toList mp
      ]

day12a :: Map Point Char :~> Int
day12a =
  MkSol
    { sParse = noFail $ parseAsciiMap Just
    , sShow = show
    , sSolve =
        noFail $ \mp ->
          sum
            [ S.size region * S.size dirRegion
            | letterRegions <- toList $ regions mp
            , region <- NES.toSet <$> toList letterRegions
            , dirRegion <- toList $ regionNeighbors region
            ]
    }

day12b :: Map Point Char :~> Int
day12b =
  MkSol
    { sParse = sParse day12a
    , sShow = show
    , sSolve =
        noFail \mp ->
          sum
            [ S.size region * S.size (contiguousRegions dirRegion)
            | letterRegions <- toList $ regions mp
            , region <- NES.toSet <$> toList letterRegions
            , dirRegion <- toList $ regionNeighbors region
            ]
    }
