-- |
-- Module      : AOC2018.Day06
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
module AOC2018.Day06 (
  day06a,
  day06b,
) where

import AOC.Common (clearOut, freqs, readAll)
import AOC.Common.Point (Point, boundingBox, mannDist)
import AOC.Solver (dyno_, (:~>) (..))
import Control.Monad (guard, (<=<))
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.Ix (range)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import Linear (V2 (..))
import Witherable (catMaybes, mapMaybe)

type Box = V2 Point

bbPoints :: Box -> [Point]
bbPoints (V2 mins maxs) = range (mins, maxs)

labelVoronoi ::
  -- | set of sites
  NonEmpty Point ->
  -- | point to label
  Point ->
  -- | the label, if unique
  Maybe Point
labelVoronoi sites p = do
  (closestSite, _) :| [] <-
    Just
      . NE.head
      . NE.groupWith1 snd
      . NE.sortWith snd
      $ dists
  pure closestSite
  where
    dists = sites <&> \site -> (site, mannDist p site)

day06a :: NonEmpty Point :~> Int
day06a =
  MkSol
    { sParse = (NE.nonEmpty <=< traverse parseLine) . lines
    , sShow = show
    , sSolve = \sites ->
        Just $
          let bb = boundingBox sites
              voron =
                catMaybes
                  . M.fromSet (labelVoronoi sites)
                  . S.fromList
                  . bbPoints
                  $ bb
              edges =
                S.fromList
                  . mapMaybe (\(point, site) -> site <$ guard (onEdge bb point))
                  . M.toList
                  $ voron
           in maximum . freqs . M.filter (`S.notMember` edges) $ voron
    }
  where
    onEdge :: Box -> Point -> Bool
    onEdge (V2 xMin yMin `V2` V2 xMax yMax) (V2 x y) =
      x `elem` [xMin, xMax]
        || y `elem` [yMin, yMax]

day06b :: NonEmpty Point :~> Int
day06b =
  MkSol
    { sParse = (NE.nonEmpty <=< traverse parseLine) . lines
    , sShow = show
    , sSolve = \sites ->
        Just
          . length
          . filter ((< dyno_ "lim" 10000) . (`totalDist` sites))
          . bbPoints
          . boundingBox
          $ sites
    }
  where
    totalDist p = sum . fmap (mannDist p)

parseLine :: String -> Maybe Point
parseLine =
  (packUp =<<)
    . readAll
    . words
    . clearOut (not . isDigit)
  where
    packUp [x, y] = Just $ V2 x y
    packUp _ = Nothing
