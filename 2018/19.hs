#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Heap (MinHeap)
import qualified Data.Heap as H
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ord
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators

import           Util.Util

type Mem = Vector Int

data Opcode = Addr | Addi
            | Mulr | Muli
            | Banr | Bani
            | Borr | Bori
            | Setr | Seti
            | Gtir | Gtri | Gtrr
            | Eqir | Eqri | Eqrr
  deriving (Show, Eq, Ord, Read, Enum)

data Op = Op Opcode Int Int Int
  deriving (Show, Eq, Ord)

stepMem :: Op -> Mem -> Mem
stepMem (Op op a b c) mem = case op of
  Addr -> set c (reg a + reg b)
  Addi -> set c (reg a + b)
  Mulr -> set c (reg a * reg b)
  Muli -> set c (reg a * b)
  Banr -> set c (reg a .&. reg b)
  Bani -> set c (reg a .&. b)
  Borr -> set c (reg a .|. reg b)
  Bori -> set c (reg a .|. b)
  Setr -> set c (reg a)
  Seti -> set c (a)
  Gtir -> set c (if a > reg b then 1 else 0)
  Gtri -> set c (if reg a > b then 1 else 0)
  Gtrr -> set c (if reg a > reg b then 1 else 0)
  Eqir -> set c (if a == reg b then 1 else 0)
  Eqri -> set c (if reg a == b then 1 else 0)
  Eqrr -> set c (if reg a == reg b then 1 else 0)
  where
    reg i
      | i >= 0 && i < V.length mem = mem V.! i
      | otherwise = error "out of bounds"
    set i x
      | i >= 0 && i < V.length mem = V.take i mem <> V.singleton x <> V.drop (i+1) mem
      | otherwise = error "out of bounds"

data VM = VM {
  vmMem :: Mem
  }
  deriving (Show, Eq)

data Program = Program {
  progIpp :: Int,
  progCode :: Vector Op
  }
  deriving (Show, Eq)

macro1 = V.fromList [Op Seti 1 1 2,Op Mulr 4 2 5,Op Eqrr 5 3 5,Op Addr 5 1 1,Op Addi 1 1 1,
                     Op Addr 4 0 0,Op Addi 2 1 2,Op Gtrr 2 3 5,Op Addr 1 5 1,Op Seti 2 4 1]
macro1f (VM mem) = VM (mem V.// [(0, v0), (1, ip)])
  where
    v0 = if (mem V.! 3) `mod` (mem V.! 4) == 0 then
           (mem V.! 0) + (mem V.! 4)
         else
           (mem V.! 0)
    ip = (mem V.! 1) + 10

step :: Program -> VM -> VM
step (Program ipp prog) (VM mem)
  | V.take 10 (V.drop ip prog) == macro1 = macro1f (VM mem)
  | inRange = let mem' = stepMem (prog V.! ip) mem
                  ip' = mem' V.! ipp + 1
                  mem'' = mem' V.// [(ipp, ip')]
              in (VM mem'')
  | otherwise = VM mem
  where
    ip = mem V.! ipp
    inRange = ip >= 0 && ip < V.length prog

run :: Program -> VM -> VM
run prog vm = if vm' /= vm then
                run prog vm'
              else
                vm
  where
    vm' = step prog vm

pInput :: (Monad m, CharParsing m) => m Program
pInput = Program <$> pIpp <*> (V.fromList <$> some pOp)
  where
    pIpp = string "#ip" *> spaces *> number <* spaces
    pOp = (Op
           <$> (pOpcode <* spaces)
           <*> (number <* spaces)
           <*> (number <* spaces)
           <*> (number <* spaces))
    pOpcode = read . over _head toUpper <$> some letter

main :: IO ()
main = do
  txt <- readFile "input/19.txt"
  let input = parse pInput txt
  print input

  print (run input (VM $ V.replicate 6 0))
  print (run input (VM $ V.replicate 6 0 V.// [(0, 1)]))
