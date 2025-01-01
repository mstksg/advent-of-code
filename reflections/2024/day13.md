This one reduces to basically solving two linear equations, but it's kind of
fun to see what the *linear* haskell library gives us to make things more
convenient.

Basically for `xa`, `ya`, `xb`, `yb`, we want to solve the matrix equation `M p
= c` for `p`, where `c` is our target `<x, y>`, and `M` is `[ xa xb; ya yb ]`.  We're
going to assume that our two buttons are linearly independent (they are not
multiples of each other).  Note that the `M` matrix is the transpose of the
numbers as we originally parse them.

Normally we can solve this as `p = M^-1 C`, where `M^-1 = [ yb -xb; -ya xa] /
(ad - bc)`. However, we only care about integer solutions. This means that we
can do some checks:

1.  Compute `det = ad - bc` and a matrix `U = [yb -xb ; -ya xa]`, which is
    `M^-1 * det`.
2.  Compute `p*det = U c`
3.  Check that `det` is not 0
4.  Check that ``(`mod` det)`` is 0 for all items in `U c`
5.  Our result is then the ``(`div` det)`` for all items in `U c`.

*linear* has the `det22` method for the determinant of a 2x2 matrix, but it
doesn't quite have the `M^-1 * det` function, it only has `M^-1` for
`Fractional` instances.  So we can write our own:

```haskell
-- | Returns det(A) and inv(A)det(A)
inv22Int :: (Num a, Eq a) => M22 a -> Maybe (a, M22 a)
inv22Int m@(V2 (V2 a b) (V2 c d))
  | det == 0 = Nothing
  | otherwise = Just (det, V2 (V2 d (-b)) (V2 (-c) a))
  where
    det = det22 m

type Point = V2 Int

getPrize :: V2 Point -> Point -> Maybe Int
getPrize coeff targ = do
  (det, invTimesDet) <- inv22Int (transpose coeff)
  let resTimesDet = invTimesDet !* targ
      V2 a b = (`div` det) <$> resTimesDet
  guard $ all ((== 0) . (`mod` det)) resTimesDet
  pure $ 3 * a + b

part1 :: [(V2 Point, Point)] -> Int
part1 = sum . mapMaybe (uncurry getPrize)

part2 :: [(V2 Point, Point)] -> Int
part2 = part2 . map (second (10000000000000 +))
```

Here we take advantage of `transpose`, `det22`, `!*` for matrix-vector
multiplication, the `Functor` instance of vectors for `<$>`, the `Foldable`
instance of vectors for `all`, and the `Num` instance of vectors for numeric
literals and `+`.
