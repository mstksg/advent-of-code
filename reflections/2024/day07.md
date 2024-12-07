This one works out well as a list monad based search. Essentially you are
picking operations where:

```haskell
targ == (x ? y) ? z
```

and if those `?` operations induce a list monad split, you can then search all
of the possible choices:

```haskell
checkEquation :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
checkEquation ops targ xs = targ `elem` foldl1M branchOnOp xs
  where
    branchOnOp a b = map (\f -> f a b) ops
```

Then you can do `checkEquation [(+),(*)]` for part 1 and `checkEquation
[(+),(*),cat]` for part 2.

However, it is kind of helpful to work backwards from the target to see if you
can get the initial number.  For example, in `292: 11 6 16 20`, you can
eliminate `*` as an option for the _final_ operation right off the bat.

So really, you can rephrase the problem as:

```haskell
x == y ? (z ? targ)
```

where `?` are the inverse operations, but you have some way to easily eliminate
operations that don't make sense.

```haskell
checkBackEquation :: [Int -> Int -> Maybe Int] -> Int -> [Int] -> Bool
checkBackEquation unOps targ (x:xs) = x `elem` foldrM branchOnUnOp targ xs
  where
    branchOnUnOp a b = mapMaybe (\f -> f a b) unOPs
```

And our un-ops are:

```haskell
unAdd :: Int -> Int -> Maybe Int
unAdd x y = [y - x | y >= x]

unMul :: Int -> Int -> Maybe Int
unMul x y = [y `div` x | y `mod` x == 0]

unCat :: Int -> Int -> Maybe Int
unCat x y = [d | m == x]
  where
    pow = length . takeWhile (< x) $ iterate (* 10) 1
    (d, m) = y `divMod` (10 ^ pow)
```

So part 1 is `checkBackEquation [unAdd, unMul]` and part 2 is
`checkBackEquation [unAdd, unMul, unCat]`.

Timing-wise, moving from forwards to backwards brought my times for part 2 from
380ms to 1.2ms.
