{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- |
-- Module      : AOC2025.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day10 (
  day10a,
  day10b,
)
where

import AOC.Solver ((:~>) (..))
import Control.Monad (filterM, guard, unless, (<=<))
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Finite (Finite, finites, shift, strengthen, weaken)
import Data.Foldable (asum, for_, toList, traverse_)
import Data.Functor ((<&>))
import Data.Interval (Boundary (..), Extended (..), Interval, (<=..<), (<=..<=))
import qualified Data.Interval as IV
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable (for)
import Data.Type.Ord (OrderingI (..))
import qualified Data.Vector.Mutable.Sized as SMV
import qualified Data.Vector.Sized as SV
import GHC.TypeNats (KnownNat, cmpNat, type (+), type (-))
import Safe (initMay, lastMay, minimumMay)
import Text.Read (readMaybe)

day10a :: [([Bool], [[Int]], [Int])] :~> Int
day10a =
  MkSol
    { sParse = traverse parseMe . lines
    , sShow = show
    , sSolve = fmap sum . traverse go
    }
  where
    parseMe :: String -> Maybe ([Bool], [[Int]], [Int])
    parseMe ('[' : xs) = do
      (a, ']' : bs) <- pure $ span (/= ']') xs
      let lit = map (== '#') a
          ps = words bs
      buttons <- traverse (traverse readMaybe . splitOn "," <=< initMay . drop 1) =<< initMay ps
      targets <- traverse readMaybe . splitOn "," =<< initMay . drop 1 =<< lastMay ps
      pure (lit, buttons, targets)
    parseMe _ = Nothing
    go :: ([Bool], [[Int]], [Int]) -> Maybe Int
    go (targ, buttons, _) =
      minimumMay
        [ length onButts
        | onButts <- filterM (const [False, True]) buttons
        , foldr (symmetricDifference . S.fromList) S.empty onButts == targSet
        ]
      where
        targSet = S.fromList $ map fst $ filter snd $ zip [0 ..] targ

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference x y = (x <> y) `S.difference` (x `S.intersection` y)

day10b :: [([Bool], [[Int]], [Int])] :~> Integer
day10b =
  MkSol
    { sParse = sParse day10a
    , sShow = show
    , sSolve = fmap sum . traverse go
    }
  where
    go (_, buttons, targ) = withSizedButtons buttons targ \b t ->
      let reduced = runST do
            mat <- traverse SV.thaw $ toAugMat b (fromIntegral <$> t)
            stepFullGauss' mat
            reduceBack' mat
            traverse (fmap reduceGCD . SV.freeze) mat
          maxSearch = fromIntegral $ maximum t
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

-- | n = number of lights (rows), m = number of buttons (cols)
withSizedButtons ::
  [[Int]] ->
  [Int] ->
  (forall n m. (KnownNat n, KnownNat m) => SV.Vector m (Set (Finite n)) -> SV.Vector n Int -> r) ->
  r
withSizedButtons buttons target f =
  SV.withSizedList buttons \buttons' ->
    SV.withSizedList target $
      f (S.fromList . map fromIntegral <$> buttons')

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

nextFin :: KnownNat n => Finite n -> Maybe (Finite n)
nextFin = strengthen . shift

-- | Assumes everything left of the index is already upper triangular/row
-- echelon.
stepGauss' ::
  forall n m a s.
  (KnownNat n, KnownNat m, Integral a) =>
  SV.Vector n (SMV.MVector (m + 1) s a) ->
  (Finite n, Finite m) ->
  ST s (Maybe (Finite n, Finite m))
stepGauss' mat (i, j) = do
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

stepFullGauss' ::
  (KnownNat n, KnownNat m, Integral a) =>
  SV.Vector n (SMV.MVector (m + 1) s a) ->
  ST s ()
stepFullGauss' mat = go (0, 0)
  where
    go = traverse_ go <=< stepGauss' mat

reduceGCD :: (Foldable t, Functor t, Integral b) => t b -> t b
reduceGCD xs
  | null xs' = xs
  | otherwise = (`div` foldr1 gcd xs') <$> xs
  where
    xs' = filter (/= 0) $ toList xs

-- | Zero out all elements that are not pivots or free variables
reduceBack' ::
  forall n m a s.
  (KnownNat n, KnownNat m, Integral a) =>
  SV.Vector n (SMV.MVector (m + 1) s a) ->
  ST s ()
reduceBack' mat = do
  for_ (tails (reverse finites)) \case
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

-- | Assumes 'reduceBack': all items are zero except for pivots and free
-- variables
--
-- For each equation (c, cs), xs . cs must be non-negative and a multiple of
-- c. The first item returned is uniquely the objective function, though all
-- constraints also apply to it as well.
withFrees ::
  forall n m a r.
  (KnownNat m, Integral a) =>
  SV.Vector n (SV.Vector (m + 1) a) ->
  ( forall q.
    KnownNat q =>
    NonEmpty (a, SV.Vector (q + 1) a) ->
    r
  ) ->
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

-- | Bounds for each of the non-negative variables so that the equation can be
-- non-negative.
linearBounds ::
  forall q a. (KnownNat q, Integral a) => SV.Vector (q + 1) a -> SV.Vector q (Interval a)
linearBounds v = SV.imap go coeffs
  where
    coeffs = SV.take @q v
    constTerm = SV.last v
    poss = S.fromList [i | (i, c) <- toList (SV.indexed coeffs), c > 0]
    hasOtherPos i = not (S.null (S.delete i poss))
    go i coeff
      | hasOtherPos i = Finite 0 <=..< PosInf
      | coeff > 0 = Finite (max 0 (ceilDiv (-constTerm) coeff)) <=..< PosInf
      | coeff < 0 = if ub < 0 then IV.empty else Finite 0 <=..<= Finite ub
      | constTerm < 0 = IV.empty
      | otherwise = Finite 0 <=..< PosInf
      where
        ub = constTerm `div` (-coeff)

ceilDiv :: Integral a => a -> a -> a
ceilDiv n d = (n + d - 1) `div` d

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

enumIntervalClamped :: Integral a => a -> Interval a -> [a]
enumIntervalClamped maxSearch iv = fromMaybe [] do
  lo <- case IV.lowerBound' iv of
    (NegInf, _) -> Just 0
    (Finite a, Closed) -> Just (max 0 a)
    (Finite a, Open) -> Just (max 0 (a + 1))
    (PosInf, _) -> Nothing
  hi <- case IV.upperBound' iv of
    (PosInf, _) -> Just maxSearch
    (Finite b, Closed) -> Just (min maxSearch b)
    (Finite b, Open) -> Just (min maxSearch (b - 1))
    (NegInf, _) -> Nothing
  pure [lo .. hi]
