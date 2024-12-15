-- |
-- Module      : AOC2024.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day14 (
  day14a,
  day14b,
)
where

import AOC.Common (freqs, strictIterate, (!!!))
import AOC.Common.Parser (pDecimal, parseMaybe', sepByLines, sequenceSepBy)
import AOC.Common.Point (Point)
import AOC.Solver (noFail, type (:~>) (..))
import Control.Lens (view)
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable1 (foldMap1)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Semigroup (Arg (Arg), Max (Max))
import Data.Traversable (for)
import qualified Data.Vector.Storable as VS
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))

step :: Point -> Point -> Point
step v x = mod <$> (x + v) <*> V2 101 103

day14a :: [V2 Point] :~> Int
day14a =
  MkSol
    { sParse =
        parseMaybe' . sepByLines . for (V2 "p=" "v=") $ \q ->
          q *> sequenceSepBy (V2 pDecimal pDecimal) ","
    , sShow = show
    , sSolve =
        noFail \pvs ->
          let V2 ps vs = VS.fromList <$> sequenceA pvs
           in score $ strictIterate (VS.zipWith step vs) ps !!! 100
    }
  where
    score = product . freqs . mapMaybe quadrant . VS.toList
      where
        quadrant (V2 x y) = do
          qx <- classify $ compare x 50
          qy <- classify $ compare y 51
          pure (qx, qy)
        classify = \case
          LT -> Just False
          EQ -> Nothing
          GT -> Just True

day14b :: [V2 Point] :~> Int
day14b =
  MkSol
    { sParse = sParse day14a
    , sShow = show
    , sSolve =
        \pvs -> do
          let V2 ps vs = VS.fromList <$> sequenceA pvs
              firstSteps = zip [0 ..] . strictIterate (VS.zipWith step vs) $ ps
          xi <- maxMargin $ second (map (view _x) . VS.toList) <$> take 101 firstSteps
          yi <- maxMargin $ second (map (view _y) . VS.toList) <$> take 103 firstSteps
          pure $ (xi + ((yi - xi) * 5151)) `mod` 10403
    }
  where
    maxMargin :: [(Int, [Int])] -> Maybe Int
    maxMargin = fmap (unMax . foldMap1 go) . NE.nonEmpty
      where
        unMax (Max (Arg _ i)) = i
        go (i, xs) = Max (Arg sos i)
          where
            sos = sum . fmap (join (*)) . freqs $ xs
