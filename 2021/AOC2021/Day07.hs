-- |
-- Module      : AOC2021.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
module AOC2021.Day07 (
  day07a,
  day07b,
) where

import AOC.Common (freqs, readAll, triangleNumber)
import AOC.Common.Search (binaryFindMin)
import AOC.Solver ((:~>) (..))
import Control.Monad (guard)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Semigroup (Sum (..))
import qualified Data.Vector as V

day07 ::
  -- | loss function
  (Int -> Int) ->
  [Int] :~> Int
day07 f =
  MkSol
    { sParse = readAll . splitOn ","
    , sShow = show
    , sSolve = \xs -> do
        let xsMap = freqs xs
            findFuelFor targ = getSum $ M.foldMapWithKey (\x n -> Sum $ f (abs (targ - x)) * n) xsMap
        (minX, _) <- M.lookupMin xsMap
        (maxX, _) <- M.lookupMax xsMap
        let fuelVector = V.generate (maxX + 1 - minX) $ \i -> findFuelFor (i + minX)
        binaryFindMin
          ( \x ->
              let fX = fuelVector V.! x
                  fX1 = fuelVector V.! (x + 1)
               in fX <$ guard (fX1 > fX)
          )
          minX
          maxX
    }

day07a :: [Int] :~> Int
day07a = day07 id

day07b :: [Int] :~> Int
day07b = day07 triangleNumber
