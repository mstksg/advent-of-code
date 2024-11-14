-- |
-- Module      : AOC2019.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
module AOC2019.Day09 (
  day09a,
  day09b,
) where

import AOC.Solver ((:~>) (..))
import AOC.Util (eitherToMaybe)
import AOC2019.Common.Intcode (IErr, Memory, parseMem, stepForever, yieldAndDie)
import Control.Monad (join)
import Data.Conduino (await, runPipe, (.|))

runProg :: Int -> Memory -> Either IErr (Maybe Int)
runProg i m =
  runPipe $
    yieldAndDie i
      .| stepForever m
      .| await

day09a :: Memory :~> Int
day09a =
  MkSol
    { sParse = parseMem
    , sShow = show
    , sSolve = join . eitherToMaybe . runProg 1
    }

day09b :: Memory :~> Int
day09b =
  MkSol
    { sParse = parseMem
    , sShow = show
    , sSolve = join . eitherToMaybe . runProg 2
    }
