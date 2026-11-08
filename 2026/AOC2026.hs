{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC2026
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
module AOC2026 (
  module AOC,
  challengeBundle2026,
)
where

import AOC.Discover
import AOC.Run
import AOC.Run.Interactive
import AOC2026.Day01 as AOC
import AOC2026.Day02 as AOC
import AOC2026.Day03 as AOC
import AOC2026.Day04 as AOC
import AOC2026.Day05 as AOC
import AOC2026.Day06 as AOC
import AOC2026.Day07 as AOC
import AOC2026.Day08 as AOC
import AOC2026.Day09 as AOC
import AOC2026.Day10 as AOC
import AOC2026.Day11 as AOC
import AOC2026.Day12 as AOC

challengeBundle2026 :: ChallengeBundle
challengeBundle2026 = CB 2026 $ mkChallengeMap $$(solutionList)
