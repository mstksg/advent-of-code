import System.IO.Unsafe
import Control.Monad

input :: [Integer]
input = unsafePerformIO (map read . words <$> readFile "input/01.txt")

part1 :: Integer
part1 = head $ do
  a <- input
  b <- input
  guard (a + b == 2020)
  return (a * b)

part2 :: Integer
part2 = head $ do
  a <- input
  b <- input
  c <- input
  guard (a + b + c == 2020)
  return (a * b * c)
