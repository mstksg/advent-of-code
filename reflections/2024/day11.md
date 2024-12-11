Today's "one trick" seems to be realizing that the actual ordered "list" is a
red herring: a number's progression doesn't depend on any of its neighbors. So
we really want a function `growTo :: Int -> Int -> Int`, that takes a
starting number, a number of steps to progress, and the final length that
number yields after that many steps.

Structured like this, it becomes a classic dynamic programming puzzle, because
ie `growTo 52 75` is just `growTo 5 74 + growTo 2 75`, which are all
memoizable. We can use the data-memocombinators library to structure the
dynamic programming memoization:

```haskell
growTo :: Int -> Int -> Int
growTo = Memo.memo2 Memo.integral Memo.integral go
  where
    go _ 0 = 1
    go n k = sum [ growTo m (k - 1) | m <- step n ]

step :: Int -> [Int]
step c
  | c == 0 = [1]
  | even pow = let (a, b) = c `divMod` (10 ^ (pow `div` 2))
  |             in [a, b]
  | otherwise = [c * 2024]
  where
    pow = numDigits c

part1 :: [Int] -> Int
part1 = sum . map (`growTo` 25)

part2 :: [Int] -> Int
part2 = sum . map (`growTo` 75)
```
