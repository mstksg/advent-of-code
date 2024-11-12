This year's day 1 was definitely a little spicier than most!  But Haskell's
stream processing works well for us, again.

First let's get a nice utility function to make a number the first and last
seen digit:

```haskell
firstAndLast :: [Int] -> Int
firstAndLast xs = head xs * 10 + last xs
```

And now we have to figure out how to extract the digits from each part.  For
part 1, we need only to extract all of the `isDigit` characters:

```haskell
extract1 :: String -> [Int]
extract1 = map digitToInt . filter isDigit
```

For part 2, it's a little bit trickier: we can look at each position and see if
it is a new number string:

```haskell
extract2 :: String -> [Int]
extract2 = mapMaybe isNumberString . tails
  where
    isNumberString :: String -> Maybe Int
    isNumberString str = listToMaybe
      [ d
      | (numStr, d) <-
          [ ("one",1), ("two",2), ("three",3)
          , ("four",4), ("five",5), ("six",6)
          , ("seven",7), ("eight",8), ("nine",9)
          ]
      | numStr `isPrefixOf` str
      ]
```

Note that `tails` is a function that takes `"abc"` and returns
`["abc","bc","c"]`, allowing us to look at each position in order.

And that gives us the solutions:

```haskell
day01 :: (String -> [Int]) -> String -> Int
day01 extractor = sum . map (sum . extractor) . lines

day01a = day01 extract1
day01b = day01 extract2
```

