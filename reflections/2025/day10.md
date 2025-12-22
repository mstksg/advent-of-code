For part 1, we just need to check if the mutual `xor`-ing of each index
(represented as an `IntSet` of "on" indices) yields the result or not. A nice
little trick, `filterM (\_ -> [False, True])` is a `[a] -> [[a]]` powerset
function that gives all inclusion/exclusion combinations.

```haskell
go :: [Bool] -> [[Int]] -> Int
go targ buttons =
  minimum
    [ length onButts
    | onButts <- filterM (const [False, True]) buttons
    , foldr (symmetricDifference . IS.fromList) IS.empty onButts == targSet
    ]
  where
    targSet = IS.fromList [i | (i, True) <- zip [0 ..] targ]

symmetricDifference :: IntSet -> IntSet -> IntSet
symmetricDifference x y = (x <> y) `IS.difference` (x `IS.intersection` y)
```

For part 2...hoo boy. I really wanted to do this in a way that:

1.  Required no external linear programming libraries
2.  Stayed completely in `Integer`, without going into floats or arbitrary
    precision rationals

In the end I found a way that was made nice using [sized
vectors](https://hackage.haskell.org/package/vector-sized) and [safe indices](https://hackage.haskell.org/package/finite-typelits)
to help manage everything.

I have gone on record saying that the advantage of sized vectors and safe
indexing is not the safety, but rather the guidance that they provide:
imagine if your IDE autocomplete could also tell you what indices in scope were
valid for your vectors! That being said, because the benefit is in the writing
process, it doesn't quite show as much in the finished product.

Anyway let's pull our buttons and targets into the world of type-level sizes:

```haskell
-- | n = number of lights (rows), m = number of buttons (cols)
withSizedButtons ::
  [[Int]] ->
  [Int] ->
  (forall n m. (KnownNat n, KnownNat m) => SV.Vector m (Set (Finite n)) -> SV.Vector n Integer -> r) ->
  r
withSizedButtons buttons target f =
  SV.withSizedList buttons \buttons' ->
    SV.withSizedList target $
      f (S.fromList . map fromIntegral <$> buttons') . fmap fromIntegral
```

This basically moves our `[[Int]]` and `[Int]` into the type-safe world,
bringing in `m` (the number of buttons) and `n` (the number of lights) and
`Set (Finite n)`, the set of lights that each button presses. From that we can
make our type-safe augmented matrix:

```haskell
-- | n = number of lights (rows), m = number of buttons (cols)
toAugMat ::
  (KnownNat n, KnownNat m, Num a) =>
  SV.Vector m (Set (Finite n)) ->
  SV.Vector n a ->
  SV.Vector n (SV.Vector (m + 1) a)
toAugMat buttons target = SV.generate \i -> SV.generate \j ->
  case strengthen j of
    Nothing -> target `SV.index` i
    Just j'
      | i `S.member` (buttons `SV.index` j') -> 1
      | otherwise -> 0
```

`strengthen :: Finite (n + 1) -> Maybe (Finite n)` returns `Nothing` if we are
the last index, and `Just` with the `Finite n` if we aren't. For our augmented
matrix with `m + 1` columns, that means the last column will be our target.

Next we can do Guassian elimination on the mutable rows:

```haskell
-- | Assumes everything left of the index is already upper triangular/row
-- echelon.
stepGauss ::
  forall n m a s.
  (KnownNat n, KnownNat m, Integral a) =>
  SV.Vector n (SMV.MVector (m + 1) s a) ->
  (Finite n, Finite m) ->
  ST s (Maybe (Finite n, Finite m))
stepGauss mat (i, j) = do
  pivot <-
    runMaybeT $
      asum $
        [ MaybeT $ SMV.read (mat `SV.index` k) (weaken j) <&> \pVal -> (k, pVal) <$ guard (pVal /= 0)
        | k <- [i ..]
        ]
  case pivot of
    Nothing -> pure ((i,) <$> nextFin j)
    Just (pIx, pVal) -> do
      unless (pIx == i) do
        pivotRow <- SMV.clone $ mat `SV.index` pIx
        SMV.copy (mat `SV.index` pIx) (mat `SV.index` i)
        SMV.copy (mat `SV.index` i) pivotRow
      for_ (drop 1 [i ..]) \k -> do
        elimVal <- SMV.read (mat `SV.index` k) (weaken j)
        unless (elimVal == 0) do
          let common = pVal `lcm` elimVal
              pFac = common `div` elimVal
              elimFac = common `div` pVal
          for_ finites \l ->
            SMV.read (mat `SV.index` i) l >>= \x ->
              flip (SMV.modify (mat `SV.index` k)) l \y ->
                pFac * y - elimFac * x
      pure ((,) <$> nextFin i <*> nextFin j)

stepFullGauss ::
  (KnownNat n, KnownNat m, Integral a) =>
  SV.Vector n (SMV.MVector (m + 1) s a) ->
  ST s ()
stepFullGauss mat = go (0, 0)
  where
    go = traverse_ go <=< stepGauss mat
```

We have an under-determined system, and so if we have M buttons and N lights,
that means we have `Q = M - N` free variables that we can tweak. My goal is to
rephrase everything into a system equation on those `Q` free variables, so
first we eliminate all contributions that aren't free variables or pivots:

```haskell
-- | Zero out all elements that are not pivots or free variables
reduceBack ::
  forall n m a s.
  (KnownNat n, KnownNat m, Integral a) =>
  SV.Vector n (SMV.MVector (m + 1) s a) ->
  ST s ()
reduceBack mat = for_ (tails (reverse finites)) \case
  [] -> pure ()
  i : js -> do
    pivot <-
      runMaybeT . asum $
        [ MaybeT $ SMV.read (mat `SV.index` i) k <&> \pVal -> (k, pVal) <$ guard (pVal /= 0) | k <- finites
        ]
    for_ pivot \(pIx, pVal) -> do
      for_ js \j -> do
        elimVal <- SMV.read (mat `SV.index` j) pIx
        unless (elimVal == 0) $ do
          let common = pVal `lcm` elimVal
              pFac = common `div` elimVal
              elimFac = common `div` pVal
          for_ finites \l ->
            SMV.read (mat `SV.index` i) l >>= \x ->
              flip (SMV.modify (mat `SV.index` j)) l \y ->
                pFac * y - elimFac * x
```

Now we can generate our final equations only on `q`. Assume `x_4`, `x_5` free,
we would turn an equation like `7 x_1 + 3 x_4 - 2 x_5 = 20` into a row
represented by `7 x_1 = -3 x_4 + 2 x_5 + 20`, represented as the row `(7, [-3 2
20])`. Our choice of free variables is constrainted so that `[x3 2 20] . [x_4 x_5 1]`
is non-negative and a multiple of 7. Finally we can also add up all of `x_0 +
x_1 + ..` etc. to get our objective function, which is _also_ constrained to be
non-negative and a multiple of whatever the mutual LCM of all of the pivots is.

Since the constraints and the objective function are basically the same thing,
we can return them all in a non-empty list and treat the first item as the
"final objective" function which is the answer: how many times do you press
each button? (times the returned multiple)

```haskell
-- | Assumes 'reduceBack: all items are zero except for pivots and free
-- variables
--
-- For each equation (c, cs), xs . cs must be non-negative and a multiple of
-- c. The first item returned is uniquely the objective function, though all
-- constraints also apply to it as well.
withFrees ::
  forall n m a r.
  (KnownNat m, Integral a) =>
  SV.Vector n (SV.Vector (m + 1) a) ->
  (forall q. KnownNat q => NonEmpty (a, SV.Vector (q + 1) a) -> r) ->
  r
withFrees augMat f = SV.withSizedList freeVars $ \(freeVarsVec :: SV.Vector q (Finite m)) ->
  let ineqs :: [(a, SV.Vector (q + 1) a)]
      ineqs =
        [ (abs pivot, (* signum pivot) <$> v')
        | v <- toList augMat
        , let v' = SV.generate \j ->
                case strengthen j of
                  Nothing -> SV.last v
                  Just j' -> negate $ v `SV.index` weaken (freeVarsVec `SV.index` j')
        , pivot <- take 1 . filter (/= 0) $ toList v
        ]
      bigLCM = foldr1 lcm $ fst <$> ineqs
      go (pVal, coeffs) acc = acc + ((* multiple) <$> coeffs)
        where
          multiple = bigLCM `div` pVal
      objective :: SV.Vector (q + 1) a
      objective = foldr go (SV.generate $ maybe 0 (const bigLCM) . strengthen) ineqs
   in f $ (bigLCM, objective) :| ineqs
  where
    pivots :: [Finite m]
    pivots = mapMaybe (SV.findIndex (/= 0) . SV.take @m) (toList augMat)
    freeVars :: [Finite m]
    freeVars = S.toList $ S.fromAscList finites `S.difference` S.fromList pivots
```

Now we can see that each constraint vector gives us an interval over each
variable, based on the inequality that it is non-negative. We can use the
[data-interval](https://hackage.haskell.org/package/data-inteveral) library to
help manage them.

```haskell
-- | Bounds for each of the non-negative variables so that the equation can be
-- non-negative.
linearBounds ::
  forall q a. (KnownNat q, Integral a) => SV.Vector (q + 1) a -> SV.Vector q (Interval a)
linearBounds v = SV.imap go coeffs
  where
    coeffs = SV.take @q v
    constTerm = SV.last v
    poss = S.fromList [i | (i, c) <- toList (SV.indexed coeffs), c > 0]
    go i coeff
      | hasOtherPos = Finite 0 <=..< PosInf
      | coeff > 0 = Finite (max 0 (ceilDiv (-constTerm) coeff)) <=..< PosInf
      | coeff < 0 = if ub < 0 then IV.empty else Finite 0 <=..<= Finite ub
      | constTerm < 0 = IV.empty
      | otherwise = Finite 0 <=..< PosInf
      where
        hasOtherPos = not (S.null (S.delete i poss))
        ub = constTerm `div` (-coeff)

ceilDiv :: Integral a => a -> a -> a
ceilDiv n d = (n + d - 1) `div` d

```

And my final strategy is to do this:

1.  Use `linearBounds` to find the range of the first free variable
2.  Enumerate over each item in that bound:
    a.  `substitueFirstFree` to substitute the picked value as that first value
    b.  Repeat the entire thing sub-recursively, since that substitution
        greatly constrains the other variables

Eventually we have a big loop on the first variable, a small loop on the second
variable, and pretty much only a very few options on the third, ideally.

```haskell
-- | Resolve the first free variable into a fixed portion and delete it
substituteFirstFree ::
  (KnownNat q, Num a) =>
  a ->
  SV.Vector (q + 2) a ->
  SV.Vector (q + 1) a
substituteFirstFree x v = SV.generate \j ->
  (v `SV.index` shift j)
    + case strengthen j of
      Nothing -> (v `SV.index` 0) * x
      Just _ -> 0

-- | Enumerate all points in the feasible region in depth first search, by
-- fixing the first point and enumerating over the possible next points,
-- recursively.
enumFeasible ::
  forall q a.
  (KnownNat q, Integral a, Show a) =>
  a ->
  NonEmpty (SV.Vector (q + 1) a) ->
  [SV.Vector q a]
enumFeasible maxSearch constraints = case cmpNat (Proxy @q) (Proxy @0) of
  LTI -> []
  EQI -> [SV.empty]
  GTI -> case cmpNat (Proxy @1) (Proxy @q) of
    LTI -> go @(q - 1)
    EQI -> go @(q - 1)
    GTI -> []
  where
    go :: forall u. (u + 1 ~ q) => [SV.Vector (u + 1) a]
    go =
      enumIntervalClamped maxSearch firstBound >>= \x ->
        let newConstraints :: NonEmpty (SV.Vector (u + 1) a)
            newConstraints = substituteFirstFree @u x <$> constraints
         in SV.cons x <$> enumFeasible maxSearch newConstraints
      where
        bounds :: SV.Vector (u + 1) (Interval a)
        bounds = foldr1 (SV.zipWith IV.intersection) (linearBounds <$> constraints)
        firstBound :: Interval a
        firstBound = SV.head @u bounds
```

The whole thing wrapped up:

```haskell
solveButtons :: [[Int]] -> [Int] -> Maybe Integer
solveButtons buttons targ = withSizedButtons buttons targ \b t ->
  let reduced = runST do
        mat <- traverse SV.thaw $ toAugMat b t
        stepFullGauss mat
        reduceBack mat
        traverse (fmap reduceGCD . SV.freeze) mat
      maxSearch = maximum t
   in withFrees reduced $ \constrs ->
        minimumMay $
          [ obj
          | x <- enumFeasible maxSearch $ snd <$> constrs
          , let xVec = x `SV.snoc` 1
          , obj :| _ <- for constrs \(c, v) -> do
              let res = sum $ SV.zipWith (*) xVec v
              (res', 0) <- pure $ res `divMod` c
              pure res'
          ]
```
