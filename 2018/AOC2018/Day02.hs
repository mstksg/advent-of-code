-- |
-- Module      : AOC2018.Day02
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
module AOC2018.Day02 (
  day02a,
  day02b,
) where

import AOC.Common (freqs, perturbations)
import AOC.Solver ((:~>) (..))
import Control.Monad (guard)
import Data.Containers.ListUtils (nubOrd)
import Data.List (find)
import qualified Data.Map as M
import qualified Data.Set as S
import Witherable (catMaybes)

-- | We compute a frequency map of all of the characters in a string, and
-- then get all of the frequencies that happened for each line.
--
-- Then we build a frequency map of the frequencies!
day02a :: [String] :~> Int
day02a =
  MkSol
    { sParse = Just . lines
    , sShow = show
    , sSolve =
        mulTwoThree -- > lookup how many times
        --     2 and 3 occurred, and
        --     multiply
          . freqs -- > build a frequency map of
          --     all seen frequencies
          . concatMap -- > get the frequency map of
            (nubOrd . M.elems . freqs) --     each string, and then
            --     combine all of the
            --     frequencies into a big
            --     list of frequencies
    }
  where
    mulTwoThree m = (*) <$> M.lookup 2 m <*> M.lookup 3 m

-- | The main work is in 'firstNeighbor', which looks thorugh a list of
-- items and finds the first item whose neighbor was already seen.
--
-- Then we take the two "almost matching" strings and filter out all of the
-- characters that aren't the same, using 'zipWith' and 'catMaybes'.
day02b :: [String] :~> String
day02b =
  MkSol
    { sParse = Just . lines
    , sShow = id
    , sSolve =
        fmap (uncurry onlySame)
          . firstNeighbor
    }
  where
    onlySame xs = catMaybes . zipWith (\x y -> x <$ guard (x == y)) xs

-- | Find the first string in a list that is a neighbor of a previous
-- string.
firstNeighbor :: [String] -> Maybe (String, String)
firstNeighbor = go S.empty
  where
    go seen (x : xs) = case find (`S.member` seen) (neighbors x) of
      Just n -> Just (x, n)
      Nothing -> go (x `S.insert` seen) xs
    go _ [] = Nothing

-- | Get all one-character neighbors of a given string
neighbors :: String -> [String]
neighbors = perturbations (const ['a' .. 'z'])
