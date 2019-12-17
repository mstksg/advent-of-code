{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Char
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
import           System.IO.Unsafe

import           AStar
import           IntCode
import           Util

prog :: IntCode
prog = unsafePerformIO $ makeProg . map read . splitOn "," . strip <$> readFile "input/17.txt"

neighbors :: V2 Int -> [V2 Int]
neighbors p = [p + V2 dx dy | dx <- [-1, 0, 1], dy <- [-1, 0, 1],
               abs dx + abs dy == 1]

turnLeft :: V2 Int -> V2 Int
turnLeft = negate . perp

turnRight :: V2 Int -> V2 Int
turnRight = perp

solve1 :: IntCode -> Int
solve1 prog = sum [x*y | V2 x y <- Set.toList scaffold,
                   all (\p -> Set.member p scaffold) (neighbors (V2 x y))]
  where
    txt = map chr (runProg prog [])
    page = lines txt
    scaffold = Set.fromList [ V2 x y | (y, line) <- zip [0..] page,
                              (x, c) <- zip [0..] line,
                              c `elem` ['#', '^', '<', '>', 'v']]

replaceL :: Eq a => [a] -> [a] -> [a] -> [a]
replaceL xs ys [] = []
replaceL xs ys abc
  | take n abc == xs   = ys ++ replaceL xs ys (drop n abc)
  | otherwise = head abc : (replaceL xs ys (tail abc))
  where
    n = length xs

solve2 :: IntCode -> Int
solve2 prog = last $ runProg (poke 0 2 prog) inputs
  where
    txt = map chr (runProg prog [])
    page = lines txt
    scaffold = Set.fromList [ V2 x y | (y, line) <- zip [0..] page,
                              (x, c) <- zip [0..] line,
                              c `elem` ['#', '^', '<', '>', 'v']]
    robotpos = head [ V2 x y | (y, line) <- zip [0..] page,
                      (x, c) <- zip [0..] line,
                      c == '^']
    robotdir = V2 0 (-1)

    -- Input to intcode
    inputs = map ord (
      (unlines $ map (intercalate ",") [main, partA, partB, partC])
      ++ "n\n")

    -- Brute force compress the path into A,B,C
    (main, partA, partB, partC) = head (
      do nA <- [1..20]
         let partA = take nA path
             path' = replaceL partA ["A"] path
         guard (length (intercalate "," partA) <= 20)
         nB <- [1..20]
         let partB = take nB (dropWhile (=="A") path')
             path'' = replaceL partB ["B"] path'
         guard (not $ elem "A" partB)
         guard (length (intercalate "," partB) <= 20)
         let ab c = elem c ["A", "B"]
             partC = takeWhile (not . ab) $ dropWhile ab path''
             path''' = replaceL partC ["C"] path''
         guard (all (\c -> elem c ["A", "B", "C"]) path''')
         guard (length (intercalate "," partC) <= 20)
         return (path''', partA, partB, partC)
      )

    -- Compute the path the robot needs to take
    path = filter (/= "0") $ go robotpos robotdir
    go pos dir = let path = takeWhile (`Set.member` scaffold) (iterate (+dir) pos)
                     end = last path
                     steps = length path - 1
                     turn
                       | Set.member (end + turnRight dir) scaffold = Just 'R'
                       | Set.member (end + turnLeft dir) scaffold = Just 'L'
                       | otherwise = Nothing
                 in case turn of
                      Just 'R' -> [show steps, "R"] <> go end (turnRight dir)
                      Just 'L' -> [show steps, "L"] <> go end (turnLeft dir)
                      Nothing -> [show steps]
