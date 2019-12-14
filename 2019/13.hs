{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Foldable
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
import           Linear.V2

import           IntCode
import           Util

prog :: IO IntCode
prog = makeProg . map read . splitOn "," . strip <$> readFile "input/13.txt"

solve1 :: IntCode -> Int
solve1 prog = go (step prog) Map.empty
  where
    go HaltF screen = count 2 screen
    go (OutputF x (OutputF y (OutputF t k))) screen
      = go k (Map.insert (V2 x y) t screen)

draw :: Map (V2 Int) Int -> Int -> IO ()
draw screen score = do
  let xmin = minimum (map (view _x) (Map.keys screen))
      ymin = minimum (map (view _y) (Map.keys screen))
      xmax = maximum (map (view _x) (Map.keys screen))
      ymax = maximum (map (view _y) (Map.keys screen))
      c 0 = " "
      c 1 = "#"
      c 2 = "\ESC[31m▒\ESC[m"
      c 3 = "\ESC[32m█\ESC[m"
      c 4 = "\ESC[34mo\ESC[m"
  T.putStrLn $ T.pack $ unlines [
    fold [c (Map.findWithDefault 0 (V2 x y) screen) | x <- [xmin..xmax]]
    | y <- [ymin..ymax]]
  threadDelay (2 * 1000)

solve2 :: IntCode -> IO ()
solve2 prog = go (step prog { memory = IntMap.insert 0 2 (memory prog)}) Map.empty 0
  where
    go HaltF screen score = print score
    go (OutputF x (OutputF y (OutputF t k))) screen score
      | (x, y) == (-1, 0) = go k screen t
      | otherwise = go k (Map.insert (V2 x y) t screen) score
    go (InputF k) screen score = do
      draw screen score
      let
        ball = head [p | (p, v) <- Map.toList screen, v == 4]
        paddle = head [p | (p, v) <- Map.toList screen, v == 3]
        joystick
          | view _x (ball - paddle) > 0 = 1
          | view _x (ball - paddle) < 0 = -1
          | view _x (ball - paddle) == 0 = 0
      go (k joystick) screen score

main :: IO ()
main = do
  p <- prog
  print (solve1 p)
  solve2 p
