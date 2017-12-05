#!/usr/bin/runhaskell

import           Control.Monad
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.List

data State = State !Int !(IntMap Int)
  deriving (Eq, Ord, Show)

start :: [Int] -> State
start xs = State 0 (IM.fromAscList (zip [0..] xs))

update :: (Int -> Int) -> State -> State
update u (State pc im) = case IM.lookup pc im of
                         Just o -> State (pc + o) (IM.insert pc (u o) im)
                         Nothing -> error "Out of bounds"

run :: (Int -> Int) -> State -> Int
run u s@(State pc im) = case IM.lookup pc im of
                        Just _ -> 1 + run u (update u s)
                        Nothing -> 0

main :: IO ()
main = do
  mem <- map read . lines <$> readFile "input.05"
  print (run (+1) (start mem))
  print (run (\o -> if o >= 3 then o - 1 else o + 1) (start mem))
