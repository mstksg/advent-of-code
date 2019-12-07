{-# LANGUAGE RecordWildCards #-}

import           Control.Monad
import           Data.List
import           Data.Vector (Vector)
import qualified Data.Vector as V

data IntCode = IntCode {
  memory :: Vector Int,
  pc :: Int
  }
  deriving Show

data Effect = Input (Int -> Effect)
            | Output Int Effect
            | HaltEff

data Param = Imm Int | Pos Int
  deriving Show

data Op = Halt | Add | Mul | In | Out | JNZ | JZ | TestLT | TestEQ

decodeInstruction :: Vector Int -> Int -> (Op, [Param])
decodeInstruction mem pc = case (mem V.! pc) `mod` 100 of
  99 -> (Halt, [])
  1 -> (Add, arity 3)
  2 -> (Mul, arity 3)
  3 -> (In, arity 1)
  4 -> (Out, arity 1)
  5 -> (JNZ, arity 2)
  6 -> (JZ, arity 2)
  7 -> (TestLT, arity 3)
  8 -> (TestEQ, arity 3)
  where
    modes = [case c of {'0' -> Pos; '1' -> Imm} | c <- reverse $ show ((mem V.! pc) `div` 100)] ++ repeat Pos
    arity n = zipWith id modes [mem V.! (pc + i) | i <- [1..n]]

step :: IntCode -> Effect
step m@IntCode{..} = case instruction of
  (Halt, []) -> HaltEff
  (Add, [a, b, Pos c]) -> store c (ind a + ind b)
  (Mul, [a, b, Pos c]) -> store c (ind a * ind b)
  (In,        [Pos i]) -> Input (\val -> store i val)
  (Out,           [o]) -> Output (ind o) continue
  (JNZ, [b, t])
    | ind b /= 0       -> jump (ind t)
    | otherwise        -> continue
  (JZ, [b, t])
    | ind b == 0       -> jump (ind t)
    | otherwise        -> continue
  (TestLT, [a, b, Pos t]) -> store t (if ind a < ind b then 1 else 0)
  (TestEQ, [a, b, Pos t]) -> store t (if ind a == ind b then 1 else 0)
  where
    ind (Imm x) = x
    ind (Pos x) = memory V.! x
    instruction = decodeInstruction memory pc
    instructionSize = 1 + length (snd instruction)
    jump pc = step m{pc = pc}
    continue = step m{pc = pc + instructionSize}
    store i x = step m{pc = pc + instructionSize,
                       memory = memory V.// [(i, x)]}

prog :: Vector Int
prog = V.fromList [3,8,1001,8,10,8,105,1,0,0,21,46,55,68,89,110,191,272,353,434,99999,3,9,1002,9,3,9,1001,9,3,9,102,4,9,9,101,4,9,9,1002,9,5,9,4,9,99,3,9,102,3,9,9,4,9,99,3,9,1001,9,5,9,102,4,9,9,4,9,99,3,9,1001,9,5,9,1002,9,2,9,1001,9,5,9,1002,9,3,9,4,9,99,3,9,101,3,9,9,102,3,9,9,101,3,9,9,1002,9,4,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99]

run :: IntCode -> [Int] -> [Int]
run mem ins = go (step mem) ins
  where
    go (Input f) ins = go (f (head ins)) (tail ins)
    go (Output o f) ins = o : go f ins
    go HaltEff ins = []

runAmp :: Int -> Int -> Int
runAmp phase i = head (runAmp' phase [i])

runAmp' :: Int -> [Int] -> [Int]
runAmp' phase ins = run (IntCode {memory = prog, pc = 0}) (phase : ins)

solve1 :: Int
solve1 = maximum $ do
  phases <- permutations [0..4]
  let out = foldl' (\v p -> runAmp p v) 0 phases
  return out

solve2 :: Int
solve2 = maximum $ do
  [a, b, c, d, e] <- permutations [5..9]
  let
    out_a = runAmp' a (0 : out_e)
    out_b = runAmp' b out_a
    out_c = runAmp' c out_b
    out_d = runAmp' d out_c
    out_e = runAmp' e out_d
  return (last out_e)
