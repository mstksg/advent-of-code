#!/usr/bin/env stack
-- stack runghc

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Functor.Identity
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
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           System.IO.Unsafe
import qualified Text.Parsec as Parsec
import           Text.Parser.Char
import           Text.Parser.Combinators

input :: (Int, Int)
input = (493, 71863)

runGame :: Int -> Int -> (Map Int Int, Seq Int, Int)
runGame numPlayers lastMarble = let initialScores = Map.fromList [(n, 0) | n <- [0..numPlayers-1]]
                                    initialCircle = Seq.singleton 0
                                in go initialScores initialCircle 0 1
  where
    go scores circle current turn
      | turn <= lastMarble = if turn > 0 && mod turn 23 == 0 then
          -- player gets this marble added to their score
          -- plus the one 7 spaces back, which is deleted
          let
            player = turn `mod` numPlayers
            current' = (current - 7) `mod` Seq.length circle
            scores' = Map.adjust (+(turn + Seq.index circle current')) player scores
            circle' = Seq.deleteAt current' circle
          in go scores' circle' current' (turn + 1)
        else
          -- New marble inserted before the marble two spaces to the right.
          let
            current' = (current + 2) `mod` Seq.length circle
            circle' = Seq.insertAt current' turn circle
          in go scores circle' current' (turn + 1)
      | otherwise = (scores, circle, current)

highScore :: Int -> Int -> Int
highScore numPlayers lastMarble = case runGame numPlayers lastMarble of
                                    (scores, _, _) -> maximum scores

solve1 :: Int
solve1 = highScore (fst input) (snd input)

solve2 :: Int
solve2 = highScore (fst input) (snd input * 100)

main :: IO ()
main = do
  print solve1
  print solve2
