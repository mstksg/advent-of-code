This one is a cute little interpreter problem, a staple of advent of code.
Let's write Part 1 in a way that makes Part 2 easy, where we will have to
eventually "run" it backwards. We can use `Finite n` as the type with `n`
inhabitants, so `Finite 8` will, for example, have the numbers 0 to 7. And also
`Vector n a` from `Data.Vector.Sized`, which contains `n` items.

```haskell
data Combo
  = CLiteral (Finite 4)
  | CReg (Finite 3)

data Instr
  = ADV Combo
  | BXL (Finite 8)
  | BST Combo
  | JNZ (Finite 4)
  | BXC
  | OUT Combo
  | BDV Combo
  | CDV Combo
```

We can then write a function to interpret the outputs into a monoid.

```haskell
stepWith ::
  Monoid a =>
  Vector 8 Instr ->
  -- | out
  (Finite 8 -> a) ->
  -- | Starting a
  Word ->
  -- | Starting b
  Word ->
  -- | Starting c
  Word ->
  a
stepWith prog out = go 0
  where
    go i !a !b !c = case prog `SV.index` i of
      ADV r -> withStep go (a `div` (2 ^ combo r)) b c
      BXL l -> withStep go a (b `xor` fromIntegral l) c
      BST r -> withStep go a (combo r `mod` 8) c
      JNZ l
        | a == 0 -> withStep go 0 b c
        | otherwise -> go (weakenN l) a b c   -- weakenN :: Finite 4 -> Finite 8
      BXC -> withStep go a (b `xor` c) c
      OUT r ->
        let o = modulo (fromIntegral (combo r))
         in out o <> withStep go a b c
      BDV r -> withStep go a (a `div` (2 ^ combo r)) c
      CDV r -> withStep go a b (a `div` (2 ^ combo r))
      where
        combo = \case
          CLiteral l -> fromIntegral l
          CReg 0 -> a
          CReg 1 -> b
          CReg _ -> c
        withStep p
          | i == maxBound = \_ _ _ -> mempty
          | otherwise = p (i + 1)
```

Part 1 is a straightforward application, although we can use a difflist to get
O(n) concats instead of O(n^2)

```haskell
import Data.DList as DL

part1 :: Vector 8 Instr -> Word -> Word -> Word -> [Finite 8]
part1 prog a b c = DL.toList $ stepWith prog DL.singleton a b c
```

Part 2 it gets a bit interesting. We can solve it "in general" under the
conditions:


1.  The final instruction is JNZ 0
2.  There is one `OUT` per loop, with a register
3.  b and c are overwritten at the start of each loop

The plan would be:


1.  Start from the end with a known `a` and move backwards, accumulating all
    possible values of `a` that would lead to the end value, ignoring b and c
2.  For each of those possible a's, start from the beginning with that `a` and
    filter the ones that don't produce the correct `OUT`.

We have to write a "step backwards" from scratch, but we can actually use our
original `stepWith` to write a version that _bails_ after the first output, by
having our monoid be `Data.Monoid.First`. Then in the line `out o <> withStep
go a abc`, it'll just completely ignore the right hand side and output the
first `OUT` result.

```haskell
searchStep :: Vector 8 Instr -> [Finite 8] -> [Word]
searchStep prog outs = do
  -- enforce the invariants
  JNZ 0 <- pure $ prog `SV.index` maxBound
  [CReg _] <- pure [r | OUT r <- toList prog]
  search 0 (reverse outs)
  where
    search a = \case
      o : os -> do
        a' <- stepBack a
        guard $ stepForward a' == Just o
        search a' os
      [] -> pure a
    -- doesn't enforce that b and c are reset, because i'm lazy
    stepForward :: Word -> Maybe (Finite 8)
    stepForward a0 = getFirst $ stepWith tp (First . Just) a0 0 0
    stepBack :: Word -> [Word]
    stepBack = go' maxBound
      where
        go' i a = case tp `SV.index` i of
          ADV r -> do
            a' <- case r of
              CLiteral l -> ((a `shift` fromIntegral l) +) <$> [0 .. 2 ^ getFinite l - 1]
              CReg _ -> []
            go' (pred i) a'
          OUT _ -> pure a
          _ -> go' (pred i) a
```

We really only have to handle the `ADV r` case because that's the only
instruction that modifies `A`. If we `ADV 3`, that means that the possible
"starting A's" are `known_a * 8 + x`, where `x` is between 0 and 7.

Wrapping it all up:

```haskell
part2 :: Vector 8 Instr -> [Finite 8] -> Maybe Word
part2 instrs = listToMaybe . searchStep instrs
```
