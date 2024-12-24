This one end up being a nice hylomorphism.

We can build the upper triangle of the adjacency map: only include edges from
items to items later in the alphabet.

```haskell
connMap :: Ord a => [(a, a)] -> Map a (Set a)
connMap xs =
  M.unionsWith
    (<>)
    [ M.fromList [(a, S.singleton b), (a, S.empty)]
    | [a, b] <- xs <&> \(x, y) -> sort [x, y]
    ]
```

Part 1 we can manually unroll:

```haskell
part1 :: Map a (Set a) -> Int
part1 conns = length do
    (a, adjA) <- M.toList conns
    b <- toList adjA
    c <- toList $ (conns M.! b) `S.intersection` adjA
    guard $ any ("t" `isPrefixOf`) [a, b, c]
```

This is using the list monad's non-determinism for a depth first search:
For every item `a`, all of the items `b` in its adjacencies are valid in its
triple.  From there we can add any item `c` in the adjacencies of `b`, provided
`c` is also in `fromA`, the adjacencies from `as`.

Part 2 is where things get fun. One way to look at it is, from each starting
point, build a tree of all adjacency hops from it at are valid: each next child
they must be reachable from all of its parents. Then, collapse all branching
paths from top to bottom.

Therefore, our base functor is a list of parents to children:

```haskell
newtype Branch a = Branch { unBranch :: [(String, a)] }
  deriving Functor
```

And now we are in good shape to write our hylomorphism:

```haskell
allCliques :: Ord a => Map a (Set a) -> [[a]]
allCliques conns = hylo tearDown build (M.toList conns)
  where
    build = Branch
          . map (\(a, cands) -> (a, [(b, cands `S.intersection` (conns M.! b)) | b <- toList cands]))
    tearDown = foldMap (\(here, there) -> (here :) <$> if null there then pure [] else there)
             . unBranch

part2 :: Map a (Set a) -> [a]
part2 = maximumBy (comparing length) . allCliques
```
