{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import qualified Criterion as Cr
import qualified Criterion.Main as Cr
import           Data.Char
import           Data.Foldable
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Linear.V2
import           System.IO.Unsafe

import           AStar
import           IntCode
import           Util

prog :: IntCode
prog = makeProg [109,424,203,1,21102,1,11,0,1106,0,282,21101,18,0,0,1106,0,259,2102,1,1,221,203,1,21101,0,31,0,1106,0,282,21102,1,38,0,1106,0,259,21002,23,1,2,21202,1,1,3,21102,1,1,1,21101,57,0,0,1106,0,303,2102,1,1,222,20101,0,221,3,21002,221,1,2,21102,259,1,1,21102,1,80,0,1106,0,225,21102,96,1,2,21101,91,0,0,1105,1,303,2101,0,1,223,21001,222,0,4,21101,259,0,3,21101,225,0,2,21102,1,225,1,21101,118,0,0,1106,0,225,21002,222,1,3,21102,1,43,2,21101,0,133,0,1105,1,303,21202,1,-1,1,22001,223,1,1,21101,148,0,0,1106,0,259,1201,1,0,223,20101,0,221,4,20101,0,222,3,21101,16,0,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21101,195,0,0,106,0,109,20207,1,223,2,20101,0,23,1,21102,-1,1,3,21101,0,214,0,1105,1,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,1202,-4,1,249,22102,1,-3,1,22101,0,-2,2,21202,-1,1,3,21102,250,1,0,1106,0,225,21202,1,1,-4,109,-5,2106,0,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2105,1,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,22102,1,-2,-2,109,-3,2105,1,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,21202,-2,1,3,21101,0,343,0,1105,1,303,1106,0,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,21202,-4,1,1,21102,384,1,0,1106,0,303,1105,1,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,22102,1,1,-4,109,-5,2105,1,0]

scan prog x y = head (runProg prog [x, y])

-- scan :: IntCode -> Int -> Int -> Int
-- scan prog x = (\y -> case eff of
--                  InputF k -> case k y of
--                    OutputF z _ -> z)
--   where
--     eff = case step prog of
--       InputF k -> k x

solve1 :: IntCode -> Int
solve1 prog = sum [scan prog x y | x <- [0..49], y <- [0..49]]

solve2 :: IntCode -> Int
solve2 prog = go 0 0
  where
    go !x !y
      | scan prog x (y' + 99) == 1 = 10000 * x + y'
      | otherwise = go (x+1) y'
      where
        scanx99 = scan prog (x+99)
        y' = head $ dropWhile (\y' -> scanx99 y' == 0) [y..]

main :: IO ()
main = Cr.defaultMain [
  Cr.bench "part1" (Cr.whnf solve1 prog),
  Cr.bench "part2" (Cr.whnf solve2 prog)
  ]
