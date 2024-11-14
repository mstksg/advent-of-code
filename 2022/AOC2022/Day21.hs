{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AOC2022.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day21 (
  day21a,
  day21b,
)
where

import AOC.Solver ((:~>) (..))
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Text.Read (readMaybe)

data Op = Add | Sub | Mul | Div

runOp :: Op -> Int -> Int -> Int
runOp = \case
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div

data Expr
  = Node Op Expr Expr
  | Leaf Int
  | Var

makeBaseFunctor ''Expr

evalF :: ExprF (Maybe Int) -> Maybe Int
evalF = \case
  NodeF o x y -> runOp o <$> x <*> y
  LeafF i -> Just i
  VarF -> Nothing

day21a :: [(String, ExprF String)] :~> Int
day21a =
  MkSol
    { sParse = traverse parseLine . lines
    , sShow = show
    , sSolve = \pairs ->
        let mp = M.fromList pairs
         in hylo evalF (mp M.!) "root"
    }

-- | target, value
invOpRight :: Op -> Int -> Int -> Int
invOpRight = \case
  Add -> (-)
  Sub -> flip (-)
  Mul -> div
  Div -> flip div

-- | target, value
invOpLeft :: Op -> Int -> Int -> Int
invOpLeft = \case
  Add -> (-)
  Sub -> (+)
  Mul -> div
  Div -> (*)

-- | Right if it gives an answer, Left if it involves a Var, with a function
-- from a target number returning what the var needs to be to make the
-- expression hit that target.
evalSolveF :: ExprF (Either (Int -> Int) Int) -> Either (Int -> Int) Int
evalSolveF = \case
  NodeF o x y -> case (x, y) of
    (Right x', Right y') -> Right $ runOp o x' y'
    (Right x', Left fy) -> Left $ fy . flip (invOpRight o) x'
    (Left fx, Right y') -> Left $ fx . flip (invOpLeft o) y'
    (Left _, Left _) -> error "Unsupported: multiple variables"
  LeafF i -> Right i
  VarF -> Left id

day21b :: [(String, ExprF String)] :~> Int
day21b =
  MkSol
    { sParse = traverse parseLine . lines
    , sShow = show
    , sSolve = \pairs ->
        let mp =
              M.adjust reRoot "root" $
                M.insert "humn" VarF $
                  M.fromList pairs
         in case hylo evalSolveF (mp M.!) "root" of
              Left f -> Just $ f 0 -- make a-b = 0
              Right _ -> Nothing -- constant value, no solve
    }
  where
    reRoot = \case
      NodeF Add x y -> NodeF Sub x y
      _ -> error "root should be +"

parseLine :: String -> Maybe (String, ExprF String)
parseLine str = case splitOn ":" str of
  [k, v] ->
    (k,) <$> case readMaybe v of
      Just x -> Just $ LeafF x
      Nothing -> case words v of
        [a, "+", b] -> Just $ NodeF Add a b
        [a, "-", b] -> Just $ NodeF Sub a b
        [a, "*", b] -> Just $ NodeF Mul a b
        [a, "/", b] -> Just $ NodeF Div a b
        _ -> Nothing
  _ -> Nothing
