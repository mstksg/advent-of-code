{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC.Challenge
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
module AOC2020 (
  module AOC,
  challengeBundle2020,
)
where

import AOC.Discover
import AOC.Run
import AOC.Run.Interactive
import AOC2020.Day01 as AOC
import AOC2020.Day02 as AOC
import AOC2020.Day03 as AOC
import AOC2020.Day04 as AOC
import AOC2020.Day05 as AOC
import AOC2020.Day06 as AOC
import AOC2020.Day07 as AOC
import AOC2020.Day08 as AOC
import AOC2020.Day09 as AOC
import AOC2020.Day10 as AOC
import AOC2020.Day11 as AOC
import AOC2020.Day12 as AOC
import AOC2020.Day13 as AOC
import AOC2020.Day14 as AOC
import AOC2020.Day15 as AOC
import AOC2020.Day16 as AOC
import AOC2020.Day17 as AOC
import AOC2020.Day18 as AOC
import AOC2020.Day19 as AOC
import AOC2020.Day20 as AOC
import AOC2020.Day21 as AOC
import AOC2020.Day22 as AOC
import AOC2020.Day23 as AOC
import AOC2020.Day24 as AOC
import AOC2020.Day25 as AOC

challengeBundle2020 :: ChallengeBundle
challengeBundle2020 = CB 2020 $ mkChallengeMap $$(solutionList)
