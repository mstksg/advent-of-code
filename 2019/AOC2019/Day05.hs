{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : AOC2019.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
module AOC2019.Day05 (
  day05a,
  day05b,
) where

import AOC.Solver ((:~>) (..))
import AOC2019.Common.Intcode (IErr, Memory, parseMem, stepForever, yieldAndDie)
import Data.Conduino (runPipe, (.|))
import qualified Data.Conduino.Combinators as C
import Data.Either (fromRight)

runProg :: Int -> Memory -> Either IErr (Maybe Int)
runProg i m =
  runPipe $
    yieldAndDie i
      .| stepForever m
      .| C.last

day05a :: Memory :~> Int
day05a =
  MkSol
    { sParse = parseMem
    , sShow = show
    , sSolve = fromRight Nothing . runProg 1
    }

day05b :: Memory :~> Int
day05b =
  MkSol
    { sParse = parseMem
    , sShow = show
    , sSolve = fromRight Nothing . runProg 5
    }
