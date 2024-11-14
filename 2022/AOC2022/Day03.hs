-- |
-- Module      : AOC2022.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day03 (
  day03a,
  day03b,
)
where

import AOC.Common (charFinite)
import AOC.Solver ((:~>) (..))
import Control.Monad ((<=<))
import qualified Data.IntSet as IS
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)
import Safe (foldl1May)

priority :: Char -> Maybe Int
priority = fmap (uncurry score) . charFinite
  where
    score = \case
      False -> (+ 1) . fromIntegral
      True -> (+ 27) . fromIntegral

day03 ::
  ([String] -> [[String]]) ->
  [String] :~> Int
day03 splitter =
  MkSol
    { sParse = Just . lines
    , sShow = show
    , sSolve = fmap sum . traverse go . splitter
    }
  where
    go =
      fmap fst . IS.maxView
        <=< foldl1May IS.intersection . map (IS.fromList . mapMaybe priority)

day03a :: [String] :~> Int
day03a = day03 $ map \xs -> chunksOf (length xs `div` 2) xs

day03b :: [String] :~> Int
day03b = day03 $ chunksOf 3
