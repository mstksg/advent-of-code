#!/usr/bin/runhaskell
import Data.List

testInput :: [[Int]]
testInput = [[5, 1, 9, 5],
             [7, 5, 3],
             [2, 4, 6, 8]]

parse :: String -> [[Int]]
parse realInput = map (map read . words) (lines realInput)

checksum :: [[Int]] -> Int
checksum rows = sum [maximum row - minimum row | row <- rows]

checksum2 :: [[Int]] -> Int
checksum2 rows = sum (map (head . go . sort) rows)
  where
    go [] = []
    go (n:ms) = [m `div` n | m <- ms, gcd n m == n] ++ go ms


main :: IO ()
main = do
  parsed <- parse <$> readFile "02.input"
  print (checksum parsed)
  print (checksum2 parsed)
