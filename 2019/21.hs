{-# LANGUAGE RecordWildCards #-}
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
import           Data.IORef
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
import           Debug.Trace
import           Linear.V2
import           System.IO.Unsafe

import           AStar
import           IntCode
import           Util

data Reg = A | B | C | D | E | F | G | H | I | T | J
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Inst = And Reg Reg | Or Reg Reg | Not Reg Reg
  deriving (Show, Read, Eq, Ord)

disp :: Inst -> String
disp (And x y) = "AND " <> show x <> " " <> show y
disp (Or x y) = "OR " <> show x <> " " <> show y
disp (Not x y) = "NOT " <> show x <> " " <> show y

prog :: IntCode
prog = unsafePerformIO $ makeProg . map read . splitOn "," . strip <$> readFile "input/21.txt"

-- Tabulation of program function

type Set512 = Integer

full :: Set512
full = sum [2^i | i <- cs]
cs = [0..511] :: [Int]
compl x = full `xor` x

regA = sum [2^i | i <- cs, (.&.) i (2^0) > 0]
regB = sum [2^i | i <- cs, (.&.) i (2^1) > 0]
regC = sum [2^i | i <- cs, (.&.) i (2^2) > 0]
regD = sum [2^i | i <- cs, (.&.) i (2^3) > 0]
regE = sum [2^i | i <- cs, (.&.) i (2^4) > 0]
regF = sum [2^i | i <- cs, (.&.) i (2^5) > 0]
regG = sum [2^i | i <- cs, (.&.) i (2^6) > 0]
regH = sum [2^i | i <- cs, (.&.) i (2^7) > 0]
regI = sum [2^i | i <- cs, (.&.) i (2^8) > 0]

simulate :: [Inst] -> (Set512, Set512)
simulate xs = simulateStep xs (0, 0)
simulateStep :: [Inst] -> (Set512, Set512) -> (Set512, Set512)
simulateStep xs start = go start xs
  where
    go (t, j) [] = (t, j)
    go (t, j) (x:xs) = case x of
      (And a J) -> (t, reg a .&. j)
      (And a T) -> (reg a .&. t, j)
      (Or a J) -> (t, reg a .|. j)
      (Or a T) -> (reg a .|. t, j)
      (Not a J) -> (t, compl (reg a))
      (Not a T) -> (compl (reg a), j)
      where
        reg :: Reg -> Set512
        reg A = regA
        reg B = regB
        reg C = regC
        reg D = regD
        reg E = regE
        reg F = regF
        reg G = regG
        reg H = regH
        reg I = regI
        reg T = t
        reg J = j

simOn :: Set512 -> String -> Int
simOn vset cs = if vset .&. (2^i) > 0 then 1 else 0
  where
    bs = [if c == '#' then 1 else 0 | c <- cs] :: [Int]
    i = sum [b * bit i | (i, b) <- zip [0..8] bs]

-- Solutions

display :: [Int] -> IO ()
display output
  | last output > 256 = print (last output)
  | otherwise = putStr (map chr output)

-- solve1 :: IO ()
-- solve1 = print $ last $ runProg prog $ map ord $ unlines (map disp p <> ["WALK"])
--   where
--     p = [Not A J,
--          Not B T,
--          Or T J,
--          And D J,
--          Not C T,
--          And A T,
--          And B T,
--          And D T,
--          Or T J
--         ]

solve1 :: IO ()
solve1 = let solution = bfsOn viewCases actions goal ([], simulate []) :: ([Inst], (Set512, Set512))
         in display (execute solution)
  where
    actions (p, sim) = do f <- [And, Or, Not]
                          r1 <- [A .. D] ++ [T, J]
                          r2 <- [T, J]
                          return (p ++ [f r1 r2], simulateStep [f r1 r2] sim)
    execute (p, sim) = runProg prog $ map ord $ unlines (map disp p <> ["WALK"])

    doJump = [
      ".########",
      "#.##.####",
      "##.#..###"
      ]

    noJump = [
      "####.##.#",
      "#...#####"
      ]

    viewCases (p, (t, j)) = [simOn j cs | cs <- doJump ++ noJump]

    goal (p, (t, j)) = and [simOn j cs == 1 | cs <- doJump]
                       && and [simOn j cs == 0 | cs <- noJump]

solve2 :: IO ()
solve2 = let solution = bfsOn viewCases actions goal ([], simulate []) :: ([Inst], (Set512, Set512))
         in display (execute solution)
  where
    actions (p, sim) = do f <- [And, Or, Not]
                          r1 <- [A .. J]
                          r2 <- [T, J]
                          return (p ++ [f r1 r2], simulateStep [f r1 r2] sim)
    execute (p, sim) = runProg prog $ map ord $ unlines (map disp p <> ["RUN"])

    doJump = [
      ".########",
      "##.##.###",
      "..###.#.#",
      "##.#..###",
      "#..#.####",
      "#.##.#.##"
      ]

    noJump = [
      "##.#.##.#",
      "####..#.#",
      "###..#.##",
      "##..#.###",
      "####..##.",
      "####.##.#",
      "###.##.##",
      "###...###",
      "##...####",
      "#...#####",
      "###.#####",
      "####.####"
      ]

    viewCases (p, (t, j)) = [simOn j cs | cs <- doJump ++ noJump]

    goal (p, (t, j)) = and [simOn j cs == 1 | cs <- doJump]
                       && and [simOn j cs == 0 | cs <- noJump]

main :: IO ()
main = do
  solve1
  solve2
