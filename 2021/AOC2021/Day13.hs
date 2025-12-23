-- |
-- Module      : AOC2021.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!
module AOC2021.Day13 (
  day13a,
  day13b,
) where

import AOC.Common (listTup, listV2, readAll, traverseLines)
import AOC.Common.Point (Point, parseLetters)
import AOC.Solver (noFail, (:~>) (..))
import Control.Lens (over)
import Control.Monad ((<=<))
import Data.Bitraversable (bitraverse)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2 (V2 (..), _x, _y)
import Text.Read (readMaybe)

day13a :: ([Point], [Point]) :~> Int
day13a =
  MkSol
    { sParse =
        bitraverse
          (traverseLines $ readAll <=< listV2 . splitOn ",")
          (traverseLines $ uncurry parseFold <=< listTup . splitOn "=")
          <=< listTup
            . splitOn "\n\n"
    , sShow = show
    , sSolve = noFail \(ptList, folds) ->
        let ptSet = S.fromList ptList
         in S.size $ foldr go ptSet (take 1 folds)
    }
  where
    parseFold ax v = do
      xy <- listToMaybe (reverse ax)
      vv <- readMaybe v
      pure
        if xy == 'x'
          then V2 vv 0
          else V2 0 vv
    go axis = S.map $ \p -> abs (p - axis)

day13b :: ([Point], [(Bool, Int)]) :~> Set Point
day13b =
  MkSol
    { sParse =
        bitraverse
          (traverseLines $ readAll <=< listV2 . splitOn ",")
          (traverseLines $ uncurry parseFold <=< listTup . splitOn "=")
          <=< listTup
            . splitOn "\n\n"
    , sShow = parseLetters
    , sSolve = noFail \(ptList, folds) ->
        let ptSet = S.fromList ptList
         in foldl' (flip go) ptSet folds
    }
  where
    parseFold ax v = do
      xy <- listToMaybe (reverse ax)
      vv <- readMaybe v
      pure (xy == 'x', vv)
    go (isX, a) =
      S.map $
        let axFunc
              | isX = over _x
              | otherwise = over _y
         in axFunc $ \i -> negate (abs (i - a)) + a
