Our trusty friend `transpose` comes in handy here!

For part 1, we parse the grid and then transpose and reverse each line, matching on the `head` to get
the operation we care about:

```haskell
import Data.List (transpose)

part1 :: String -> Int
part1 = sum . map (go . reverse) . transpose . map words . lines
  where
    go ("*":xs) = product $ map read xs
    go ("+":xs) = sum $ map read xs
```

And for part 2, we actually can transpose the entire long strings to get
"columns". Splitting on blank columns, we see that they are pretty
straightforward to process:

```
["623+","431 ","  4 "]
```

We just need to grab the `+` and then add the rest. Lots of ways to do this, I
ended up using `init` (gets all but the last item) and `last` for my solve:

```haskell
import Data.Char (isSpace)
import Data.List.Split (splitWhen)

part2 :: String -> Int
part2 = sum . map go . splitWhen (all isSpace) . transpose . lines
  where
    go (x:xs) = case last x of
      '*' -> product $ map read (init x : xs)
      '+' -> sum $ map read (init x : xs)
```
