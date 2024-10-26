-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
module AOC2023.Day05 (
  day05a,
  day05b,
)
where

import AOC.Common (listTup, listTup3)
import AOC.Solver ((:~>) (..))
import Control.Lens (traverseOf, _1)
import Control.Monad ((<=<))
import Data.Interval (Interval)
import qualified Data.Interval as IV
import Data.IntervalMap.Strict (IntervalMap)
import qualified Data.IntervalMap.Strict as IVM
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IVS
import Data.List (foldl')
import Data.List.Split (chunksOf, splitOn)
import Data.Traversable (for)
import Safe (minimumMay)
import Text.Read (readMaybe)

convertSingle :: Int -> IntervalMap Int Int -> Int
convertSingle x = maybe x (x +) . IVM.lookup x

fromRange :: Int -> Int -> Interval Int
fromRange x len = IV.Finite x IV.<=..< IV.Finite (x + len)

convertMany :: IntervalSet Int -> IntervalMap Int Int -> IntervalSet Int
convertMany xs mp = misses <> hits
  where
    tempMap :: IntervalMap Int ()
    tempMap = IVM.fromList . map (,()) . IVS.toList $ xs
    misses = IVM.keysSet $ tempMap IVM.\\ mp
    hits =
      IVS.fromList
        . map (\(iv, delta) -> IV.mapMonotonic (+ delta) iv)
        . IVM.toList
        $ IVM.intersectionWith const mp tempMap

parseLine :: [String] -> Maybe ([Int], [IntervalMap Int Int])
parseLine [] = Nothing
parseLine (x : xs) = do
  initSeeds <- traverse readMaybe $ drop 1 (words x)
  ivMaps <- for (splitOn [""] (drop 1 xs)) \ys ->
    IVM.fromList
      <$> for (drop 1 ys) \y -> do
        (dest, src, len) <- listTup3 =<< traverse readMaybe (words y)
        pure (fromRange src len, dest - src)
  pure (initSeeds, ivMaps)

day05a :: ([Int], [IntervalMap Int Int]) :~> Int
day05a =
  MkSol
    { sParse = parseLine . lines
    , sShow = show
    , sSolve = \(s0, process) ->
        minimumMay
          [ foldl' convertSingle s process
          | s <- s0
          ]
    }

day05b :: ([Interval Int], [IntervalMap Int Int]) :~> Int
day05b =
  MkSol
    { sParse = traverseOf _1 pairUp <=< parseLine . lines
    , sShow = show
    , sSolve = \(s0, process) ->
        fromFinite . IV.lowerBound . IVS.span $
          foldl' convertMany (IVS.fromList s0) process
    }
  where
    pairUp = traverse (fmap (uncurry fromRange) . listTup) . chunksOf 2
    fromFinite = \case
      IV.Finite x -> Just x
      _ -> Nothing
