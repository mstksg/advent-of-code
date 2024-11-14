-- |
-- Module      : AOC2022.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day09 (
  day09a,
  day09b,
)
where

import AOC.Common (listTup, strictIterate, (!!!))
import AOC.Common.Point (Dir, Point, dirPoint, parseDir)
import AOC.Solver ((:~>) (..))
import Control.Monad ((<=<))
import Data.Bitraversable (bitraverse)
import Data.List (scanl')
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Text.Read (readMaybe)

parseLine :: String -> Maybe (Dir, Int)
parseLine =
  bitraverse (parseDir <=< listToMaybe) readMaybe
    <=< listTup . words

lag :: [Point] -> [Point]
lag = scanl' go 0
  where
    go curr new
      | maximum (fmap abs dist) > 1 = curr + signum dist
      | otherwise = curr
      where
        dist = new - curr

day09 :: Int -> [(Dir, Int)] :~> Int
day09 lagAmount =
  MkSol
    { sParse = traverse parseLine . lines
    , sShow = show
    , sSolve =
        Just
          . S.size
          . S.fromList
          . (!!! lagAmount)
          . strictIterate lag
          . scanl' (+) 0
          . concatMap (\(d, i) -> replicate i (dirPoint d))
    }

day09a :: [(Dir, Int)] :~> Int
day09a = day09 1

day09b :: [(Dir, Int)] :~> Int
day09b = day09 9
