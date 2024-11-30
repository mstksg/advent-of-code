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

import AOC.Common (listTup)
import AOC.Common.Parser (CharParser, fullLine, pDecimal, pDropUntil, pTokMany, parseMaybe')
import AOC.Solver ((:~>) (..))
import Control.Lens (traverseOf, _1)
import Control.Monad ((<=<))
import qualified Control.Monad.Combinators as P
import Data.Interval (Interval)
import qualified Data.Interval as IV
import Data.IntervalMap.Strict (IntervalMap)
import qualified Data.IntervalMap.Strict as IVM
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IVS
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Safe (minimumMay)
import qualified Text.Megaparsec.Char as P

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

inpParser :: CharParser ([Int], [IntervalMap Int Int])
inpParser = do
  seeds <- fullLine $ "seeds: " *> pTokMany pDecimal
  P.newline
  imaps <- P.many do
    _ <- pDropUntil P.newline
    P.many . fullLine $ do
      dest <- pDecimal
      src <- pDecimal
      len <- pDecimal
      pure (fromRange src len, dest - src)
  pure (seeds, IVM.fromList <$> imaps)

day05a :: ([Int], [IntervalMap Int Int]) :~> Int
day05a =
  MkSol
    { sParse = parseMaybe' inpParser
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
    { sParse = traverseOf _1 pairUp <=< parseMaybe' inpParser
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
