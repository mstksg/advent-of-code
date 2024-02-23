-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!
module AOC2023.Day13 (
  day13a,
  day13b,
)
where

import AOC.Common (firstJust)
import AOC.Common.Point (Point, fillBoundingBox, parseAsciiSet)
import AOC.Solver (noFail, traverseSum, (:~>) (..))
import Control.Lens (contains, over)
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Linear.V2 (V2 (..))

findRefl :: Set Point -> [Int]
findRefl pts = findForMap cols <> map (* 100) (findForMap rows)
  where
    cols :: IntMap IntSet
    cols =
      IM.fromListWith
        (<>)
        [ (x, IS.singleton y)
        | V2 x y <- toList pts
        ]
    rows :: IntMap IntSet
    rows =
      IM.fromListWith
        (<>)
        [ (y, IS.singleton x)
        | V2 x y <- toList pts
        ]
    findForMap :: IntMap IntSet -> [Int]
    findForMap mp = flip filter [minKey + 1 .. maxKey] $ \i ->
      let (lt, gt) = IM.spanAntitone (< i) mp
       in and $ zipWith (==) (snd <$> IM.toDescList lt) (snd <$> IM.toAscList gt)
      where
        (minKey, _) = IM.findMin mp
        (maxKey, _) = IM.findMax mp

day13a :: [Set Point] :~> Int
day13a =
  MkSol
    { sParse = noFail $ map (parseAsciiSet (== '#')) . splitOn "\n\n"
    , sShow = show
    , sSolve = traverseSum $ listToMaybe . findRefl
    }

findSmudge :: Set Point -> Maybe Int
findSmudge pts = flip firstJust (fillBoundingBox pts) \pt ->
  find (`IS.notMember` origRefl)
    . findRefl
    . over (contains pt) not
    $ pts
  where
    origRefl = IS.fromList $ findRefl pts

day13b :: [Set Point] :~> Int
day13b =
  MkSol
    { sParse = noFail $ map (parseAsciiSet (== '#')) . splitOn "\n\n"
    , sShow = show
    , sSolve = traverseSum findSmudge
    }
