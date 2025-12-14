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

import AOC.Common (listV2)
import AOC.Common.Point (Point, V2 (..))
import AOC.Solver (noFail, (:~>) (..))
import Control.Monad ((<=<))
import qualified Data.Interval as IV
import qualified Data.IntervalMap.Lazy as IVM
import qualified Data.IntervalSet as IVS
import Data.List (scanl', tails)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Safe (maximumMay)
import Text.Read (readMaybe)

rectArea :: Point -> Point -> Int
rectArea x y = product do
  x' <- x
  y' <- y
  pure $ abs (x' - y') + 1

day09a :: [V2 Int] :~> Int
day09a =
  MkSol
    { sParse = traverse (listV2 <=< traverse readMaybe . splitOn ",") . lines
    , sShow = show
    , sSolve =
        noFail \pts ->
          maximum
            [ rectArea p q
            | p : ps <- tails pts
            , q <- ps
            ]
    }

-- | x coords to the y coordinates they cointain
regions :: [V2 Int] -> IVM.IntervalMap Int (IVS.IntervalSet Int)
regions pts = IVM.filter (not . IVS.null) $ IVM.fromList $ zip (drop 1 xRanges) yRanges
  where
    xs =
      M.toList $
        M.fromListWith
          (<>)
          [ (x, S.singleton y)
          | V2 x y <- pts
          ]
    (xRanges, yRanges) = unzip $ scanl' go (IV.NegInf IV.<..< IV.Finite 0, IVS.empty) xs
      where
        go (i0, curr) (x, ys) = (IV.upperBound i0 IV.<=..< IV.Finite x, curr')
          where
            curr' = (curr `IVS.union` ivs) `IVS.difference` (curr `IVS.intersection` ivs)
            ivs =
              IVS.fromList
                [ IV.Finite a IV.<=..< IV.Finite b
                | [a, b] <- chunksOf 2 (S.toList ys)
                ]

day09b :: [V2 Int] :~> Int
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
              , let xRange = IV.Finite (min px qx) IV.<=..< IV.Finite (max px qx)
                    yRange = IV.Finite (min py qy) IV.<=..< IV.Finite (max py qy)
                    region = IVM.singleton xRange (IVS.singleton yRange)
                    outOfBounds = IVM.intersectionWith IVS.difference region allowedRegion
              , all IVS.null outOfBounds
              ]
    }
