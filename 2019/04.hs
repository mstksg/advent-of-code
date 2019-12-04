import Data.List

input :: [Int]
input = [206938..679128]

solve1 = length [n | n <- input, show n == sort (show n),
                 maximum (map length (group (show n))) >= 2]

solve2 = length [n | n <- input, show n == sort (show n),
                 elem 2 (map length (group (show n)))]
