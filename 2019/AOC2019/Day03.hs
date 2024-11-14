-- |
-- Module      : AOC2019.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
module AOC2019.Day03 (
  day03a,
  day03b,
) where

import AOC.Common.Point (Dir, Point, dirPoint, mannDist, parseDir)
import AOC.Solver ((:~>) (..))
import Control.Monad ((<=<))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup (Min (..))
import Safe (scanl1Def)
import Safe.Foldable (minimumMay)
import Text.Read (readMaybe)

type Path = [(Dir, Int)]

parsePath :: String -> Maybe Path
parsePath = traverse parsePoint . splitOn ","
  where
    parsePoint (d : ns) = (,) <$> parseDir d <*> readMaybe ns
    parsePoint _ = Nothing

-- | From a list of paths, get a Map of the points where they cross, along
-- with the minimum time sum to get to that point.
crossings :: NonEmpty Path -> Map Point Int
crossings = foldr1 (M.intersectionWith (+)) . fmap follow
  where
    -- a map of every point visted to the steps taken to visit it
    follow :: Path -> Map Point Int
    follow =
      M.fromListWith min
        . flip zip [1 ..]
        . scanl1Def [] (+)
        . concatMap (uncurry expandDir)
    expandDir d ns = replicate ns (dirPoint d)

day03a :: NonEmpty Path :~> Int
day03a =
  MkSol
    { sParse = NE.nonEmpty <=< traverse parsePath . lines
    , sShow = show
    , sSolve =
        fmap getMin
          . foldMap (Just . Min . mannDist 0)
          . M.keys
          . crossings
    }

day03b :: NonEmpty Path :~> Int
day03b =
  MkSol
    { sParse = NE.nonEmpty <=< traverse parsePath . lines
    , sShow = show
    , sSolve = minimumMay . crossings
    }
