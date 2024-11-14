{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC2022
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
module AOC2022 (
  module AOC,
  challengeBundle2022,
)
where

import AOC.Discover
import AOC.Run
import AOC.Run.Interactive
import AOC2022.Day01 as AOC
import AOC2022.Day02 as AOC
import AOC2022.Day03 as AOC
import AOC2022.Day04 as AOC
import AOC2022.Day05 as AOC
import AOC2022.Day06 as AOC
import AOC2022.Day07 as AOC
import AOC2022.Day08 as AOC
import AOC2022.Day09 as AOC
import AOC2022.Day10 as AOC
import AOC2022.Day11 as AOC
import AOC2022.Day12 as AOC
import AOC2022.Day13 as AOC
import AOC2022.Day14 as AOC
import AOC2022.Day15 as AOC
import AOC2022.Day16 as AOC
import AOC2022.Day17 as AOC
import AOC2022.Day18 as AOC
import AOC2022.Day19 as AOC
import AOC2022.Day20 as AOC
import AOC2022.Day21 as AOC
import AOC2022.Day22 as AOC
import AOC2022.Day23 as AOC
import AOC2022.Day24 as AOC
import AOC2022.Day25 as AOC

challengeBundle2022 :: ChallengeBundle
challengeBundle2022 = CB 2022 $ mkChallengeMap $$(solutionList)
