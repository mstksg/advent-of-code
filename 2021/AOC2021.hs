{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC2021
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
module AOC2021 (
  module AOC,
  challengeBundle2021,
)
where

import AOC.Discover
import AOC.Run
import AOC.Run.Interactive
import AOC2021.Day01 as AOC
import AOC2021.Day02 as AOC
import AOC2021.Day03 as AOC
import AOC2021.Day04 as AOC
import AOC2021.Day05 as AOC
import AOC2021.Day06 as AOC
import AOC2021.Day07 as AOC
import AOC2021.Day08 as AOC
import AOC2021.Day09 as AOC
import AOC2021.Day10 as AOC
import AOC2021.Day11 as AOC
import AOC2021.Day12 as AOC
import AOC2021.Day13 as AOC
import AOC2021.Day14 as AOC
import AOC2021.Day15 as AOC
import AOC2021.Day16 as AOC
import AOC2021.Day17 as AOC
import AOC2021.Day18 as AOC
import AOC2021.Day19 as AOC
import AOC2021.Day20 as AOC
import AOC2021.Day21 as AOC
import AOC2021.Day22 as AOC
import AOC2021.Day23 as AOC
import AOC2021.Day24 as AOC
import AOC2021.Day25 as AOC

challengeBundle2021 :: ChallengeBundle
challengeBundle2021 = CB 2021 $ mkChallengeMap $$(solutionList)
