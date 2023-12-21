-- |
-- Module      : AOC2020.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
module AOC2020.Day23
  ( day23a,
    day23b,
  )
where

import AOC.Solver ((:~>) (..))
import Control.Monad (unless)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Class (lift)
import Data.Char (digitToInt, intToDigit)
import qualified Data.Conduino as C
import qualified Data.Conduino.Combinators as C
import Data.Foldable (for_)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV

newtype CrabState s = CrabState {csRight :: MVector s Int}

sourceCrabState ::
  (PrimMonad m, PrimState m ~ s) =>
  CrabState s ->
  -- | item to start from
  Int ->
  C.Pipe i Int u m ()
sourceCrabState CrabState {..} i0 = go i0
  where
    go i = do
      j <- lift $ MV.unsafeRead csRight i
      unless (j == i0) $ do
        C.yield j
        go j

step ::
  forall m s.
  (PrimMonad m, PrimState m ~ s) =>
  CrabState s ->
  Int ->
  m Int
step CrabState {..} lab = do
  (gs@(g1, _, g3), lab') <- pull3 lab
  MV.unsafeWrite csRight lab lab'
  let target = until (notAny gs) subWrap (subWrap lab)
  aftertarg <- MV.unsafeRead csRight target
  MV.unsafeWrite csRight target g1
  MV.unsafeWrite csRight g3 aftertarg
  pure lab'
  where
    n = MV.length csRight
    subWrap x
      | x == 0 = n - 1
      | otherwise = x - 1
    notAny (g1, g2, g3) x = x /= g1 && x /= g2 && x /= g3
    {-# INLINE notAny #-}
    pull3 :: Int -> m ((Int, Int, Int), Int)
    pull3 i0 = do
      i1 <- MV.unsafeRead csRight i0
      i2 <- MV.unsafeRead csRight i1
      i3 <- MV.unsafeRead csRight i2
      i4 <- MV.unsafeRead csRight i3
      pure ((i1, i2, i3), i4)
    {-# INLINE pull3 #-}
{-# INLINE step #-}

initialize ::
  forall m s.
  (PrimMonad m, PrimState m ~ s) =>
  Vector Int ->
  -- | initial pointer
  m (Int, CrabState s)
initialize v0 = do
  csRight <- MV.unsafeNew n
  for_ [0 .. n - 1] $ \i ->
    MV.unsafeWrite csRight (v0 V.! subWrap i) (v0 V.! i)
  let i0 = v0 V.! 0
  pure (i0, CrabState {..})
  where
    n = V.length v0
    subWrap x
      | x == 0 = n - 1
      | otherwise = x - 1
{-# INLINE initialize #-}

run ::
  (PrimMonad m, PrimState m ~ s) =>
  Int ->
  Int ->
  CrabState s ->
  m ()
run n i0 cs = go 0 i0
  where
    go !m !i
      | m == n = pure ()
      | otherwise = go (m + 1) =<< step cs i
{-# INLINE run #-}

day23a :: Vector Int :~> [Int]
day23a =
  MkSol
    { sParse = Just . V.fromList . map toIx,
      sShow = fmap intToDigit,
      sSolve = \v0 -> Just $ runST $ do
        (i0, cs) <- initialize v0
        run 100 i0 cs
        C.runPipe $
          sourceCrabState cs 0
            C..| C.map fromIx
            C..| C.sinkList
    }

day23b :: Vector Int :~> [Int]
day23b =
  MkSol
    { sParse = Just . V.fromListN 1000000 . (++ [9 ..]) . map toIx,
      sShow = show . product,
      sSolve = \v0 -> Just $ runST $ do
        (i0, cs) <- initialize v0
        run 10000000 i0 cs
        C.runPipe $
          sourceCrabState cs 0
            C..| C.map fromIx
            C..| C.take 2
            C..| C.sinkList
    }

toIx :: Char -> Int
toIx = subtract 1 . digitToInt

fromIx :: Int -> Int
fromIx = (+ 1)
