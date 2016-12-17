import Data.Foldable
import Data.Monoid
import Data.List
import Data.List.Split

type Triangle = [Integer]

solve :: [Triangle] -> String
solve tris = show (foldMap possible tris)
  where
    possible [a, b, c] = if a + b > c && a + c > b && b + c > a then
                           Sum 1
                         else mempty


part1 :: IO ()
part1 = do
  text <- readFile "input.txt"
  putStrLn $ solve [map read (words line) | line <- lines text]

part2 :: IO ()
part2 = do
  text <- readFile "input.txt"
  let rows = [map read (words line) | line <- lines text]
      tris = chunksOf 3 (fold $ transpose rows)
  putStrLn $ solve tris
