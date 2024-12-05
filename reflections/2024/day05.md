This one lends itself pretty nicely to basically topologically sorting each
page list according to the graph of "X preceeds Y" edges.

If we have a list of `(Int, Int)` rules, we can build a graph where the nodes
are the page numbers and the edges are "X preceeds Y".

Then for each page list, we can filter that graph for only the nodes in that
page list, and then toposort it:

```haskell
import qualified Data.Graph.Inductive as G

sortByRules :: [(Int, Int)] -> [Int] -> [Int]
sortByRules rules = \xs ->
    G.topsort . G.nfilter (`S.member` S.fromList xs) $ ruleGraph
  where
    ruleGraph :: G.Gr () ()
    ruleGraph =
      G.mkUGraph
        (nubOrd $ foldMap (\(x,y) -> [x,y]) rules)
        [(x, y) | V2 x y <- rules]

part1 :: [(Int, Int)] -> [[Int]] -> Int
part1 rules pages = sum
    [ middleVal orig
    | orig <- pages
    , orig == sorter orig
    ]
  where
    sorter = sortByRules rules

part2 :: [(Int, Int)] -> [[Int]] -> Int
part2 rules pages = sum
    [ middleVal sorted
    | orig <- pages
    , let sorted = sorter orig
    , orig /= sorted
    ]
  where
    sorter = sortByRules rules
```

We write `sortByRules` with a lambda closure (and name `sorters`) to ensure
that the graph is generated only once and then the closure re-applied for
every page list.

One cute way to find the middle value is to traverse the list "in parallel",
but one list twice as quickly as the other:

```haskell
middleVal :: [a] -> a
middleVal xs0 = go xs0 xs0
  where
    go (_:xs) (_:_:ys) = go xs ys
    go (x:_) _ = x
    go [] _ = error "this should return a Maybe probably"
```
