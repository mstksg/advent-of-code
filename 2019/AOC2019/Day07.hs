-- |
-- Module      : AOC2019.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
module AOC2019.Day07 (
  day07a,
  day07b,
) where

import AOC.Solver ((:~>) (..))
import AOC.Util (eitherToMaybe)
import AOC2019.Common.Intcode (
  IErr,
  Memory,
  VM,
  parseMem,
  stepForeverAndDie,
  untilHalt,
  yieldAndDie,
  yieldAndPass,
 )
import Control.Monad.Except (MonadError)
import Data.Conduino (awaitSurely, feedbackPipe, runPipe, runPipePure, (.|))
import qualified Data.Conduino.Combinators as C
import Data.List (permutations)
import Data.Semigroup (Max (..))
import Data.Void (Void)

setupChain :: MonadError IErr m => Memory -> [Int] -> VM m Void
setupChain m = foldr ((.|) . prime) (C.map id)
  where
    prime i =
      yieldAndPass i
        .| stepForeverAndDie m

day07a :: Memory :~> Int
day07a =
  MkSol
    { sParse = parseMem
    , sShow = show
    , sSolve = \m -> fmap getMax . flip foldMap (permutations [0 .. 4]) $ \xs ->
        let res =
              runPipe $
                yieldAndDie 0
                  .| setupChain m xs
                  .| awaitSurely
         in Max <$> eitherToMaybe res
    }

day07b :: Memory :~> Int
day07b =
  MkSol
    { sParse = parseMem
    , sShow = show
    , sSolve = \m -> fmap getMax . flip foldMap (permutations [5 .. 9]) $ \xs ->
        let res =
              runPipePure $
                untilHalt
                  ( yieldAndDie 0
                      .| feedbackPipe (setupChain m xs)
                  )
                  .| C.last
         in Max <$> res
    }
