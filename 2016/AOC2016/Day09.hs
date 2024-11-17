-- |
-- Module      : AOC2016.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
module AOC2016.Day09 (
  day09a,
  day09b,
) where

import AOC.Solver ((:~>) (..))
import Data.Char (isSpace)

chompWith :: (Int -> Int -> String -> Int) -> String -> Int
chompWith f = coutside 0
  where
    coutside n str = case span (/= '(') str of
      (o, []) -> n + length o
      (o, _ : is) -> cinside (n + length o) is
    cinside n str = case span (/= ')') str of
      (_, []) -> error "what"
      (i, _ : os) -> case span (/= 'x') i of
        (a, _ : b) ->
          let a' = read a
              b' = read b
              (taken, os') = splitAt (read a) os
           in coutside (n + f a' b' taken) os'
        (_, []) -> error "what"

chompOnce :: String -> Int
chompOnce = chompWith $ \x y _ -> x * y

day09a :: String :~> Int
day09a =
  MkSol
    { sParse = Just . filter (not . isSpace)
    , sShow = show
    , sSolve = Just . chompOnce
    }

chompRec :: String -> Int
chompRec = chompWith $ \_ y str -> chompRec str * y

day09b :: String :~> Int
day09b =
  MkSol
    { sParse = Just . filter (not . isSpace)
    , sShow = show
    , sSolve = Just . chompRec
    }
