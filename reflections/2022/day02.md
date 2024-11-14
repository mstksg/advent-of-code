There's a nice straightforward way to do this by just matching up all 9
combinations, but I had a bit of fun doing it algebraically using `Finite 3`, a
Haskell type that does arithmetic modulo 3, if you assume ABC and XYZ
correspond to 012, respectively.

Basically both parts 1 and 2 involve doing some modular arithmetic to get the
"shape" score, and then some modular arithmetic to get the "outcome" score.

```haskell
type Z3 = Finite 3

play
    :: (Z3 -> Z3 -> Z3)  -- ^ Get shape score
    -> (Z3 -> Z3 -> Z3)  -- ^ Get outcome score
    -> [(Z3, Z3)]
    -> Integer
play shapeScore outcomeScore = sum . map go
  where
    go (x, y) = getFinite (shapeScore x y) + 1
              + getFinite (outcomeScore x y) * 3
```

There is a bit of cute symmetry between `shapeScore` and `outcomeScore` for the
two parts.

```haskell
part1, part2 :: [(Z3, Z3)] -> Integer
part1 = play (\_ y -> y)           (\x y -> y + (1 - x))
part2 = play (\x y -> y - (1 - x)) (\_ y -> y)
```

I mostly just figured it out by using trial and error and taking advantage of
the fact that there are only so many ways you can combine two modulo 3
numbers...but there might be some nice ways to interpret them.

For example, it makes sense that the "shape score" for part 1 is literally just
your shape `y`, and the "outcome score" for part 2 is literally just your
desired outcome `y`

For the outcome score, if you assume that the answer is a "subtraction plus an
offset", then that forces the offset to be 1 in order for a match to represent
a tie.  And so for part 1, the outcome score is found by adding `1-x` to the
shape `y` in order to get the outcome.  So it makes sense that you reverse
the process for part 2: you subtract `1-x` to the outcome `y` in order to get
the shape.  I guess ???
