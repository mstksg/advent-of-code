{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC2024
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
module AOC2024 (
  module AOC,
  challengeBundle2024,
)
where

import AOC.Discover
import AOC.Run
import AOC.Run.Interactive
import AOC2024.Day01 as AOC
import AOC2024.Day02 as AOC
import AOC2024.Day03 as AOC
import AOC2024.Day04 as AOC
import AOC2024.Day05 as AOC
import AOC2024.Day06 as AOC
import AOC2024.Day07 as AOC
import AOC2024.Day08 as AOC
import AOC2024.Day09 as AOC
import AOC2024.Day10 as AOC
import AOC2024.Day11 as AOC
import AOC2024.Day12 as AOC
import AOC2024.Day13 as AOC
import AOC2024.Day14 as AOC
import AOC2024.Day15 as AOC
import AOC2024.Day16 as AOC
import AOC2024.Day17 as AOC
import AOC2024.Day18 as AOC
import AOC2024.Day19 as AOC
import AOC2024.Day20 as AOC
import AOC2024.Day21 as AOC
import AOC2024.Day22 as AOC
import AOC2024.Day23 as AOC
import AOC2024.Day24 as AOC
import AOC2024.Day25 as AOC

challengeBundle2024 :: ChallengeBundle
challengeBundle2024 = CB 2024 $ mkChallengeMap $$(solutionList)
