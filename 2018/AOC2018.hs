{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC2018
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
module AOC2018 (
  module AOC,
  challengeBundle2018,
)
where

import AOC.Discover
import AOC.Run
import AOC.Run.Interactive
import AOC2018.Day01 as AOC
import AOC2018.Day02 as AOC
import AOC2018.Day03 as AOC
import AOC2018.Day04 as AOC
import AOC2018.Day05 as AOC
import AOC2018.Day06 as AOC
import AOC2018.Day07 as AOC
import AOC2018.Day08 as AOC
import AOC2018.Day09 as AOC
import AOC2018.Day10 as AOC
import AOC2018.Day11 as AOC
import AOC2018.Day12 as AOC
import AOC2018.Day13 as AOC
import AOC2018.Day14 as AOC
import AOC2018.Day15 as AOC
import AOC2018.Day16 as AOC
import AOC2018.Day17 as AOC
import AOC2018.Day18 as AOC
import AOC2018.Day19 as AOC
import AOC2018.Day20 as AOC
import AOC2018.Day21 as AOC
import AOC2018.Day22 as AOC
import AOC2018.Day23 as AOC
import AOC2018.Day24 as AOC
import AOC2018.Day25 as AOC

challengeBundle2018 :: ChallengeBundle
challengeBundle2018 = CB 2018 $ mkChallengeMap $$(solutionList)
