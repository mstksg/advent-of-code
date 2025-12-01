Another tradition of advent of code day 1 --- everything is just a scan!

Once we parse the input into a list of integers:

```haskell
parseInp :: String -> [Int]
parseInp = read . mapMaybe rephrase . lines
  where
    rephrase 'R' = Nothing
    rephrase 'L' = Just '-'
    rephrase d = Just d
```

Then we can do the cumulative sum and count the zero's.  It actually becomes
even easier if we restrict ourselves to the integers modulo 100 using the
*finite-typelits* library and `Finite n`, using `modulo :: Integer -> Finite
n` to cast:

```haskell
part1 :: [Finite 100] -> Int
part1 = length . filter (== 0) . scanl' (+) 50
```

Part 2 you can probably do using more modulo and division tricks but the
simplest way is probably just to explode all of the ranges and do the same
counts. We use `mapAccumL` to map a stateful function, where the state is our
current position and our output is the list of all the traveled numbers:

```haskell
part2 :: [Int] -> Int
part2 = length . filter (== 0) . concat . snd . mapAccumL go 50
  where
    go curr bump
      | bump > 0 = (curr + bump, [curr + 1 .. curr + bump])
      | otherwise = (curr + bump, [curr + bump .. curr - 1])
```
