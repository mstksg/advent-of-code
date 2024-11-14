{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC2019
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
module AOC2019 (
  module AOC,
  challengeBundle2019,
)
where

import AOC.Discover
import AOC.Run
import AOC.Run.Interactive
import AOC2019.Day01 as AOC
import AOC2019.Day02 as AOC
import AOC2019.Day03 as AOC
import AOC2019.Day04 as AOC
import AOC2019.Day05 as AOC
import AOC2019.Day06 as AOC
import AOC2019.Day07 as AOC
import AOC2019.Day08 as AOC
import AOC2019.Day09 as AOC
import AOC2019.Day10 as AOC
import AOC2019.Day11 as AOC
import AOC2019.Day12 as AOC
import AOC2019.Day13 as AOC
import AOC2019.Day14 as AOC
import AOC2019.Day15 as AOC
import AOC2019.Day16 as AOC
import AOC2019.Day17 as AOC
import AOC2019.Day18 as AOC
import AOC2019.Day19 as AOC
import AOC2019.Day20 as AOC
import AOC2019.Day21 as AOC
import AOC2019.Day22 as AOC
import AOC2019.Day23 as AOC
import AOC2019.Day24 as AOC
import AOC2019.Day25 as AOC

challengeBundle2019 :: ChallengeBundle
challengeBundle2019 = CB 2019 $ mkChallengeMap $$(solutionList)
