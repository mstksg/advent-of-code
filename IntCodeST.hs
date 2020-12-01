{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module IntCodeST where

import           Control.Monad.ST
import qualified Data.HashTable.ST.Basic as H
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U

data IntCode s = IntCode {
  baseMem :: U.Vector Int,
  memory :: H.HashTable s Int Int,
  pc :: Int,
  relativeBase :: Int
  }
  deriving Show

data Effect s = InputF (Int -> ST s (Effect s))
              | OutputF Int (ST s (Effect s))
              | HaltF

data Param = Imm Int | Pos Int | Rel Int
  deriving Show

data Op = Halt | Add | Mul | In | Out | JNZ | JZ | TestLT | TestEQ | AdjustBase
  deriving Show

(?) :: IntCode s -> Int -> ST s Int
mem ? i = do
  mval <- H.lookup (memory mem) i
  pure $ case mval of
    Just x -> x
    Nothing
      | i < V.length (baseMem mem) -> baseMem mem V.! i
      | otherwise -> 0

decodeInstruction :: IntCode s -> Int -> ST s (Op, [Param])
decodeInstruction mem pc = do
  op <- (mem ? pc)
  let
    modes = [case c of {'0' -> Pos; '1' -> Imm; '2' -> Rel}
            | c <- drop 2 $ reverse $ show op] ++ repeat Pos
    arity n = zipWith id modes <$> sequence [mem ? (pc + i) | i <- [1..n]]
  sequence $ case op `mod` 100 of
    99 -> (Halt, pure [])
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

step :: IntCode s -> ST s (Effect s)
step m@IntCode{..} = do
  instruction <- decodeInstruction m pc
  let
    ind (Imm x) = pure x
    ind (Pos x) = m ? x
    ind (Rel x) = m ? (x + relativeBase)
    loc (Pos x) = x
    loc (Rel x) = x + relativeBase
    instructionSize = 1 + length (snd instruction)
    jump pc = step m{pc = pc}
    continue = step m{pc = pc + instructionSize}
    store i x = H.insert memory (loc i) x
    storeM i f ma mb = do
      a <- ma
      b <- mb
      store i (f a b)

  case instruction of
    (Halt, []) -> pure HaltF
    (Add, [a, b, c]) -> storeM c (+) (ind a) (ind b) >> continue
    (Mul, [a, b, c]) -> storeM c (*) (ind a) (ind b) >> continue
    (In,        [i]) -> pure (InputF (\val -> store i val >> continue))
    (Out,       [o]) -> do val_o <- ind o
                           pure (OutputF val_o continue)
    (JNZ, [b, t]) -> do
      val_b <- ind b
      val_t <- ind t
      if val_b /= 0 then jump val_t else continue
    (JZ, [b, t]) -> do
      val_b <- ind b
      val_t <- ind t
      if val_b == 0 then jump val_t else continue
    (TestLT, [a, b, t]) -> storeM t (\a b -> if a < b then 1 else 0) (ind a) (ind b) >> continue
    (TestEQ, [a, b, t]) -> storeM t (\a b -> if a == b then 1 else 0) (ind a) (ind b) >> continue
    (AdjustBase, [x]) -> do inc <- ind x
                            step m{pc = pc + instructionSize,
                                   relativeBase = relativeBase + inc}
    _ -> error ("Bad instruction: " ++ show instruction)

type Program = U.Vector Int

executeProg :: Program -> ST s (Effect s)
executeProg xs = do
  memory <- H.new
  step IntCode{ baseMem = xs,
                memory = memory,
                pc = 0,
                relativeBase = 0 }

makeProg :: [Int] -> Program
makeProg = V.fromList

runProg :: Program -> [Int] -> [Int]
runProg mem ins = runST (do eff <- executeProg mem
                            go ins eff)
  where
    go :: [Int] -> Effect s -> ST s [Int]
    go ins (InputF k) = k (head ins) >>= go (tail ins)
    go ins (OutputF o k) = do kval <- k
                              (o:) <$> go ins kval
    go ins HaltF = pure []

poke :: Int -> Int -> Program -> Program
poke pos val m = m V.// [(pos, val)]
