This is a good opportunity to use the ol' sliding windows trick in Haskell list
processing!

Basically if you want sliding windows over a list `"abcdefg"` to get, ie,
`["abc","bcd","cde","def","efg"]`, one thing you can do is iteratively drop items:

```haskell
map (`drop` "abcdefg")  [0,1,2]
-- ["abcdefg","bcdefg","cdefg","defg"]
```

and then take the transpose:

```
transpose $ map (`drop` "abcdefg")  [0,1,2]
-- ["abc","bcd","cde","def","efg","fg","g"]
```

You get the sliding windows, plus a few trailing elements as the window slides
off the edge of the list.

Now at this point we just need to find the first unique sliding window:

```haskell
windows :: Int -> [a] -> [[a]]
windows n xs = transpose $ map (`drop` xs) [0..n-1]

solve :: Int -> String -> Int
solve n xs = find isUnique ws + n
  where
    ws = windows n xs
    isUnique w = S.size (S.fromList w) == n


part1, part2 :: String -> Int
part1 = solve 4
part2 = solve 14
```
