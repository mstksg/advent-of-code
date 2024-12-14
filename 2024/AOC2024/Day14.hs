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
import AOC.Common.Point (Point, V2 (..), fullNeighbsSet)
import AOC.Solver (noFail, type (:~>) (..))
import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Vector.Storable as VS

day14a :: [V2 Point] :~> Int
day14a =
  MkSol
    { sParse =
        parseMaybe' $ sepByLines $ do
          p <- "p=" *> sequenceSepBy (V2 pDecimal pDecimal) ","
          v <- "v=" *> sequenceSepBy (V2 pDecimal pDecimal) ","
          pure $ V2 p v
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

step :: Point -> Point -> Point
step v x = mod <$> (x + v) <*> V2 101 103

day14b :: [V2 Point] :~> Int
day14b =
  MkSol
    { sParse = sParse day14a
    , sShow = show
    , sSolve =
        \pvs ->
          let V2 ps vs = sequenceA pvs
           in fmap fst . find (good . S.fromList . snd) . zip [0 ..] $ iterate (zipWith step vs) ps
    }
  where
    good ps = any ((`S.isSubsetOf` ps) . fullNeighbsSet) ps
