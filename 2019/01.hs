import System.IO.Unsafe

input :: [Integer]
input = unsafePerformIO (map read . words <$> readFile "input/01.txt")

fuel :: Integer -> Integer
fuel x = max 0 (div x 3 - 2)

fuel' :: Integer -> Integer
fuel' 0 = 0
fuel' x = fuel x + fuel' (fuel x)

part1 :: Integer
part1 = sum (map fuel input)

part2 :: Integer
part2 = sum (map fuel' input)
