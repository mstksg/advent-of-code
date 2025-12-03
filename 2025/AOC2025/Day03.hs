-- |
-- Module      : AOC2025.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day03 (
  day03a,
  day03b,
)
where

import AOC.Solver (noFail, (:~>) (..))
import Control.Monad (replicateM, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, modifyM)
import Data.List (tails)
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

day03 :: Int -> [String] :~> Int
day03 n =
  MkSol
    { sParse = noFail lines
    , sShow = show
    , sSolve =
        fmap sum . traverse (readMaybe @Int <=< listToMaybe . evalStateT go)
    }
  where
    go :: StateT String [] String
    go = replicateM n nextDigit

day03a :: [String] :~> Int
day03a = day03 2

day03b :: [String] :~> Int
day03b = day03 12

nextDigit :: StateT String [] Char
nextDigit = do
  n <- lift "987654321"
  modifyM \xs ->
    [ xs'
    | x : xs' <- tails xs
    , x == n
    ]
  pure n
