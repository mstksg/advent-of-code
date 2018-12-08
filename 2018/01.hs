#!/usr/bin/env stack
-- stack runghc
import           System.IO.Unsafe
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as Set

input :: String
input = unsafePerformIO (readFile "input/1.txt")

readSigned :: String -> Int
readSigned ('+':s) = read s
readSigned ('-':s) = -(read s)

solve1 :: Int
solve1 = sum (map readSigned (words input))

findrepeat xs = foldr (\x k seen -> if Set.member x seen then x else k (Set.insert x seen)) undefined xs Set.empty

solve2 = let changes = map readSigned (words input)
             freqs = scanl (+) 0 (cycle changes)
         in findrepeat freqs

main :: IO ()
main = do
  print solve1
  print solve2
