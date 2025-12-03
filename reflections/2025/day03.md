My strategy was a depth first search, basically look for 999999999999, then
999999999998, then 999999997, etc. but immediately backtrack if any of the
steps are impossible.

So, this means keeping track of a state of "what's left", and then branching
out at different digit picks -- perfect for [StateT
List](https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html)!

```haskell
nextDigit :: StateT [Int] [] Int
nextDigit = do
  n <- lift [9,8,7,6,5,4,3,2,1]
  modifyM \xs ->
    [ xs'
    | x : xs' <- tails xs
    , x == n
    ]
  pure n
```

We pick a digit non-deterministically, and then we non-deterministically chop
down our "what's left" list until we reach that digit. So `nextDigit` for
`87681` would descend to `result, state` pairs of `(8, [7,6,8,1])`, `(8, [1])`,
`(7, [6,8,1])`, `(6, [8,1])`, and `(1, [])`.  Those are our new candidates and
the associated state after picking them.  The trick is that we list them in the
order that they are most likely to yield the biggest total number.

Once we do that, we just need to `replicateM 12` to do it 12 (or 2) times:

```haskell
search :: Int -> StateT [Int] [] String
search n = map intToDigit <$> replicateM n nextDigit
```

This will perform `nextDigit` `n` times, each time chomping down more of the
string. The ones that yield no possible continuations will be pruned --
basically any time the "what's left" state gets empty, `nextDigit` will fail
for the rest of that branch.

```haskell
solve :: Int -> [String] -> Int
solve n = sum . map (read . head . evalState (search n) . map digitToInt)

part1 = solve 2
part2 = solve 12
```
