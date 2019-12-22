{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import qualified Criterion as Cr
import qualified Criterion.Main as Cr
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Debug.Trace
import           System.IO.Unsafe

import           AStar
import           IntCode
import           Util

data Reg = A | B | C | D | E | F | G | H | I | T | J
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Inst = And Reg Reg | Or Reg Reg | Not Reg Reg
  deriving (Show, Read, Eq, Ord)

-- Format for the intcode machine
format :: Inst -> String
format (And x y) = "AND " <> show x <> " " <> show y
format (Or x y) = "OR " <> show x <> " " <> show y
format (Not x y) = "NOT " <> show x <> " " <> show y

prog :: IntCode
prog = unsafePerformIO $ makeProg . map read . splitOn "," . strip <$> readFile "input/21.txt"

-- Simulate the jump decision on a given piece of terrain.
simulate :: [Inst] -> String -> (Int, Int)
simulate ops terrain = go ops 0 0
  where
    go [] t j = (t, j)
    go (op:ops) t j = case op of
      And r T -> go ops (reg r .&. t) j
      And r J -> go ops t (reg r .&. j)
      Or r T -> go ops (reg r .|. t) j
      Or r J -> go ops t (reg r .|. j)
      Not r T -> go ops (1 - reg r) j
      Not r J -> go ops t (1 - reg r)
      where
        reg T = t
        reg J = j
        reg r = case terrain !! (fromEnum r) of
                  '#' -> 1
                  '.' -> 0

simulateJ :: [Inst] -> String -> Int
simulateJ ops terrain = snd (simulate ops terrain)

-- Solutions

display :: [Int] -> IO ()
display output
  | last output > 256 = print (last output)
  | otherwise = putStr (map chr output)

execute :: [Inst] -> String -> [Int]
execute p level = runProg prog $ map ord $ unlines (map format p <> [level])

data Check a = Check [String] ([Inst] -> Bool) a
  deriving (Functor)

instance Applicative Check where
  pure = return
  (<*>) = ap

instance Monad Check where
  return a = Check [] (const True) a
  (>>=) (Check cases1 test1 a) f
    = let Check cases2 test2 b = f a
      in Check (cases1++cases2) (liftA2 (&&) test1 test2) b

jump :: String -> Check ()
jump str = Check [str] (\p -> simulateJ p str == 1) ()

noJump :: String -> Check ()
noJump str = Check [str] (\p -> simulateJ p str == 0) ()

runCheck :: Check () -> [Inst] -> Bool
runCheck (Check _ test _) p = test p

viewCases :: Check () -> [Inst] -> [Int]
viewCases (Check cases _ _) p = [simulateJ p str | str <- cases]

-- [Or A J,And C J,Not J J,And D J]
solve1 :: IO ()
solve1 = let solution = bfsOn (viewCases check) actions goal [] :: [Inst]
         in print solution >> display (execute solution "WALK")
  where
    actions p = do f <- [And, Or, Not]
                   r1 <- [A .. D] ++ [T, J]
                   r2 <- [T, J]
                   return (p ++ [f r1 r2])
    goal = runCheck check
    check = do
      jump ".########"
      jump "##.##.###"
      jump "##.#..###"
      noJump "####.##.#"
      noJump "#...#####"

-- [Not H J,Or C J,And B J,And A J,Not J J,And D J]
solve2 :: IO ()
solve2 = let solution = bfsOn (viewCases check) actions goal [] :: [Inst]
         in print solution >> display (execute solution "RUN")
  where
    actions p = do f <- [And, Or, Not]
                   r1 <- [A .. J]
                   r2 <- [T, J]
                   return (p ++ [f r1 r2])
    goal p = runCheck check p
    check = do
      jump ".########"
      jump "#.##.#.##"
      jump "#.##..###"
      jump ".#.#.#.#."
      jump "##.#..###"

      noJump "####.##.#"
      noJump "###.##.##"
      jump   "##.##.###"

      noJump "####..#.#"
      noJump "####..###"
      noJump "##.#.##.#"
      noJump "##..#.###"
      noJump "###...###"
      noJump "##...####"
      noJump "#...#####"
      noJump "###.#####"

main :: IO ()
main = do
  solve1
  solve2
  -- Cr.defaultMain [
  --   Cr.bench "solve1" (Cr.nfIO solve1),
  --   Cr.bench "solve2" (Cr.nfIO solve2)
  --   ]

-- time                 9.754 ms   (9.540 ms .. 9.937 ms)
--                      0.998 R²   (0.997 R² .. 0.999 R²)
-- mean                 9.563 ms   (9.499 ms .. 9.645 ms)
-- std dev              187.6 μs   (132.2 μs .. 254.9 μs)

-- time                 172.8 ms   (170.1 ms .. 175.0 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 174.1 ms   (173.1 ms .. 174.5 ms)
-- std dev              983.9 μs   (389.3 μs .. 1.521 ms)
