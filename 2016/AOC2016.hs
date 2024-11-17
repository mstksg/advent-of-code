{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : AOC2016
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Gather together all challenges and collect them into a single map.
module AOC2016 (
  module AOC,
  challengeBundle2016,
)
where

import AOC.Discover
import AOC.Run
import AOC.Run.Interactive
import AOC2016.Day01 as AOC
import AOC2016.Day02 as AOC
import AOC2016.Day03 as AOC
import AOC2016.Day04 as AOC
import AOC2016.Day05 as AOC
import AOC2016.Day06 as AOC
import AOC2016.Day07 as AOC
import AOC2016.Day08 as AOC
import AOC2016.Day09 as AOC
import AOC2016.Day10 as AOC
import AOC2016.Day11 as AOC
import AOC2016.Day12 as AOC
import AOC2016.Day13 as AOC
import AOC2016.Day14 as AOC
import AOC2016.Day15 as AOC
import AOC2016.Day16 as AOC
import AOC2016.Day17 as AOC
import AOC2016.Day18 as AOC
import AOC2016.Day19 as AOC
import AOC2016.Day20 as AOC
import AOC2016.Day21 as AOC
import AOC2016.Day22 as AOC
import AOC2016.Day23 as AOC
import AOC2016.Day24 as AOC
import AOC2016.Day25 as AOC

challengeBundle2016 :: ChallengeBundle
challengeBundle2016 = CB 2016 $ mkChallengeMap $$(solutionList)
