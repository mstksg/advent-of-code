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
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Util.Util

type VMState = Vector Int

data Opcode = Addr | Addi
            | Mulr | Muli
            | Banr | Bani
            | Borr | Bori
            | Setr | Seti
            | Gtir | Gtri | Gtrr
            | Eqir | Eqri | Eqrr
  deriving (Show, Eq, Ord, Read, Enum)

data Op = Op Opcode Int Int Int

step :: Op -> VMState -> VMState
step (Op op a b c) mem = case op of
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

type Example = (VMState, (Int, Int, Int, Int), VMState)

example :: (Monad m, CharParsing m) => m Example
example = do string "Before:" >> spaces
             xs <- V.fromList <$> list
             [o,a,b,c] <- endBy (read <$> some digit) spaces
             string "After:" >> spaces
             ys <- V.fromList <$> list
             return (xs, (o,a,b,c), ys)
  where
    list = between (char '[') (char ']' >> spaces) $
             sepBy (read <$> some digit) (string ", ")

program :: (Monad m, CharParsing m) => m [(Int,Int,Int,Int)]
program = some $ do [a,b,c,d] <- sepBy (read <$> some digit) (char ' ')
                    spaces
                    return (a,b,c,d)


guess :: Example -> [Opcode]
guess (before, (_,a,b,c), after) = [op | op <- [Addr .. Eqrr],
                                     step (Op op a b c) before == after]

solve1 :: [Example] -> Int
solve1 examples = length [1 | example <- examples, length (guess example) >= 3]

allOps :: Set Opcode
allOps = Set.fromList [Addr .. Eqrr]

the :: Ord a => Set a -> a
the = head . Set.toList

runProgram :: [Op] -> Int
runProgram ops = foldl (\mem op -> step op mem) (V.replicate 4 0) ops V.! 0

solve2 examples prog = runProgram program
  where
    program = map (\(o,a,b,c) -> Op (mapping Map.! o) a b c) prog

    mapping = Map.fromList (reduce consistent)
    reduce cons
      | any (\set -> Set.size set == 1) cons = let (n, o) = head [(n,the os) | (n,os) <- Map.toList cons, Set.size os == 1]
                                               in (n, o) : reduce (Set.delete o <$> (Map.delete n cons))
      | otherwise = []

    consistent = foldr (Map.unionWith Set.intersection) Map.empty (map guess' examples)
    guess' ex@(_,(o,_,_,_),_) = Map.singleton o (Set.fromList (guess ex))


main :: IO ()
main = do
  txt <- readFile "input/16.txt"
  let (examples, prog) = parse ((,) <$> some example <*> program) txt
  print (solve1 examples)
  print $ (solve2 examples prog)
