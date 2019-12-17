{-# LANGUAGE RecordWildCards #-}

module IntCode where

import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

data IntCode = IntCode {
  memory :: IntMap Int,
  pc :: Int,
  relativeBase :: Int
  }
  deriving Show

data Effect = InputF (Int -> Effect)
            | OutputF Int Effect
            | HaltF

data Param = Imm Int | Pos Int | Rel Int
  deriving Show

data Op = Halt | Add | Mul | In | Out | JNZ | JZ | TestLT | TestEQ | AdjustBase
  deriving Show

(?) :: IntMap Int -> Int -> Int
mem ? i = IntMap.findWithDefault 0 i mem

decodeInstruction :: IntMap Int -> Int -> (Op, [Param])
decodeInstruction mem pc = case (mem ? pc) `mod` 100 of
  99 -> (Halt, [])
  1 -> (Add, arity 3)
  2 -> (Mul, arity 3)
  3 -> (In, arity 1)
  4 -> (Out, arity 1)
  5 -> (JNZ, arity 2)
  6 -> (JZ, arity 2)
  7 -> (TestLT, arity 3)
  8 -> (TestEQ, arity 3)
  9 -> (AdjustBase, arity 1)
  n -> error ("Unknown opcode: " ++ show n)
  where
    modes = [case c of {'0' -> Pos; '1' -> Imm; '2' -> Rel} | c <- reverse $ show ((mem ? pc) `div` 100)] ++ repeat Pos
    arity n = zipWith id modes [mem ? (pc + i) | i <- [1..n]]

step :: IntCode -> Effect
step m@IntCode{..} = case instruction of
  (Halt, []) -> HaltF
  (Add, [a, b, c]) -> store c (ind a + ind b)
  (Mul, [a, b, c]) -> store c (ind a * ind b)
  (In,        [i]) -> InputF (\val -> store i val)
  (Out,       [o]) -> OutputF (ind o) continue
  (JNZ, [b, t])
    | ind b /= 0       -> jump (ind t)
    | otherwise        -> continue
  (JZ, [b, t])
    | ind b == 0       -> jump (ind t)
    | otherwise        -> continue
  (TestLT, [a, b, t]) -> store t (if ind a < ind b then 1 else 0)
  (TestEQ, [a, b, t]) -> store t (if ind a == ind b then 1 else 0)
  (AdjustBase, [x]) -> step m{pc = pc + instructionSize,
                              relativeBase = relativeBase + ind x}
  _ -> error ("Bad instruction: " ++ show instruction)
  where
    ind (Imm x) = x
    ind (Pos x) = memory ? x
    ind (Rel x) = memory ? (x + relativeBase)
    loc (Pos x) = x
    loc (Rel x) = x + relativeBase
    instruction = decodeInstruction memory pc
    instructionSize = 1 + length (snd instruction)
    jump pc = step m{pc = pc}
    continue = step m{pc = pc + instructionSize}
    store i x = step m{pc = pc + instructionSize,
                       memory = IntMap.insert (loc i) x memory}

makeProg :: [Int] -> IntCode
makeProg xs = IntCode { memory = IntMap.fromList (zip [0..] xs),
                        pc = 0,
                        relativeBase = 0 }

runProg :: IntCode -> [Int] -> [Int]
runProg mem ins = go (step mem) ins
  where
    go (InputF f) ins = go (f (head ins)) (tail ins)
    go (OutputF o f) ins = o : go f ins
    go HaltF ins = []

poke :: Int -> Int -> IntCode -> IntCode
poke pos val m = m { memory = IntMap.insert pos val (memory m) }
