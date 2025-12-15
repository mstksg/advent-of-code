{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC2025
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
module AOC2025 (
  module AOC,
  challengeBundle2025,
)
where

import AOC.Discover
import AOC.Run
import AOC.Run.Interactive
import AOC2025.Day01 as AOC
import AOC2025.Day02 as AOC
import AOC2025.Day03 as AOC
import AOC2025.Day04 as AOC
import AOC2025.Day05 as AOC
import AOC2025.Day06 as AOC
import AOC2025.Day07 as AOC
import AOC2025.Day08 as AOC
import AOC2025.Day09 as AOC
import AOC2025.Day10 as AOC
import AOC2025.Day11 as AOC
import AOC2025.Day12 as AOC

challengeBundle2025 :: ChallengeBundle
challengeBundle2025 = CB 2025 $ mkChallengeMap $$(solutionList)
