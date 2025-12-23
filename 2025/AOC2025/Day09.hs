-- |
-- Module      : AOC2025.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day09 (
  day09a,
  day09b,
)
where

import AOC.Common (listV2, readAll)
import AOC.Common.Point (Point, V2 (..))
import AOC.Solver ((:~>) (..))
import Control.Monad ((<=<))
import Data.Interval (Extended (..), Interval, (<..<), (<=..<))
import qualified Data.Interval as IV
import Data.IntervalMap.Lazy (IntervalMap)
import qualified Data.IntervalMap.Lazy as IVM
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IVS
import Data.List (scanl', tails)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Safe (maximumMay)

rectArea :: Point -> Point -> Int
rectArea x y = product do
  x' <- x
  y' <- y
  pure $ abs (x' - y') + 1

day09a :: [Point] :~> Int
day09a =
  MkSol
    { sParse = traverse (listV2 <=< readAll . splitOn ",") . lines
    , sShow = show
    , sSolve =
        \pts ->
          maximumMay
            [ rectArea p q
            | p : ps <- tails pts
            , q <- ps
            ]
    }

-- | x coords to the y coordinates they cointain
regions :: [Point] -> IntervalMap Int (IntervalSet Int)
regions pts = IVM.fromList $ zip (drop 1 xRanges) yRanges
  where
    xs :: [(Int, Set Int)]
    xs =
      M.toList $
        M.fromListWith
          (<>)
          [ (x, S.singleton y)
          | V2 x y <- pts
          ]
    xRanges :: [Interval Int]
    yRanges :: [IntervalSet Int]
    (xRanges, yRanges) = unzip $ scanl' go (NegInf <..< Finite 0, IVS.empty) xs
      where
        go (i0, curr) (x, ys) = (IV.upperBound i0 <=..< Finite x, curr')
          where
            curr' = (curr `IVS.union` ivs) `IVS.difference` (curr `IVS.intersection` ivs)
            ivs =
              IVS.fromList
                [ Finite a <=..< Finite b
                | [a, b] <- chunksOf 2 (S.toList ys)
                ]

day09b :: [Point] :~> Int
day09b =
  MkSol
    { sParse = sParse day09a
    , sShow = show
    , sSolve = \pts ->
        let allowedRegion = regions pts
         in maximumMay
              [ rectArea p q
              | p@(V2 px py) : ps <- tails pts
              , q@(V2 qx qy) <- ps
              , let xRange = Finite (min px qx) <=..< Finite (max px qx)
                    yRange = Finite (min py qy) <=..< Finite (max py qy)
                    region = IVM.singleton xRange (IVS.singleton yRange)
                    outOfBounds = IVM.intersectionWith IVS.difference region allowedRegion
              , all IVS.null outOfBounds
              ]
    }
