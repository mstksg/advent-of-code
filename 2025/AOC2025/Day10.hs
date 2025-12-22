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

import AOC.Common
import AOC.Solver ((:~>) (..))
import Control.Monad (filterM, foldM, guard, (<=<))
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Bitraversable
import Data.Finite (Finite, finites, shift, strengthen, weaken)
import Data.Foldable
import Data.Foldable1 hiding (foldr1, head, maximum)
import Data.Functor ((<&>))
import Data.Interval (Boundary (..), Extended (..), Interval, (<..<), (<=..<), (<=..<=))
import qualified Data.Interval as IV
import Data.IntervalMap.Lazy (IntervalMap)
import qualified Data.IntervalMap.Lazy as IVM
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IVS
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable
import Data.Tuple (swap)
import Data.Type.Ord
import qualified Data.Vector.Sized as SV
import Debug.Trace
import GHC.TypeNats
import GHC.TypeNats (KnownNat, type (+))
import qualified Linear as L
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
      let rowEchelon = execState stepFullGauss $ toAugMat b (fromIntegral <$> t)
          reduced = execState reduceBack rowEchelon
          maxSearch = fromIntegral $ maximum t
       in withFrees reduced $ \constrs' obj' ->
            minimumMay $
              [ xObj
              | x <- enumFeasible maxSearch . fmap snd $ obj' :| constrs'
              , let xVec = x `SV.snoc` 1
              , xObj :| _ <- for (obj' :| constrs') \(c, v) -> do
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
-- echelon. Returns whether or not we found a pivot here
stepGauss ::
  forall n m a.
  (KnownNat n, KnownNat m, Num a, Eq a) =>
  (Finite n, Finite m) ->
  State (SV.Vector n (SV.Vector (m + 1) a)) (Maybe (Finite n, Finite m), Bool)
stepGauss (i, j) = state \mat ->
  let origRow = mat `SV.index` i
      pivotIx = SV.findIndex (\(k, row) -> k >= i && (row `SV.index` weaken j) /= 0) $ SV.indexed mat
   in case pivotIx of
        Nothing -> (((i,) <$> nextFin j, False), mat)
        Just p ->
          let pivotRow = mat `SV.index` p
              mat' = mat SV.// [(i, pivotRow), (p, origRow)]
              pVal = pivotRow `SV.index` weaken j
              mat'' =
                SV.indexed mat' <&> \(k, row) ->
                  let elimVal = row `SV.index` weaken j
                   in if k <= i
                        then row
                        else SV.zipWith (\x y -> pVal * y - elimVal * x) pivotRow row
           in (((,) <$> nextFin i <*> nextFin j, True), mat'')

reduceGCD :: (Foldable t, Functor t, Integral b) => t b -> t b
reduceGCD xs
  | null xs' = xs
  | otherwise = (* firstSign) . (`div` foldr1 gcd xs') <$> xs
  where
    xs' = filter (/= 0) $ toList xs
    firstSign = signum $ head xs'

stepFullGauss ::
  (KnownNat n, KnownNat m, Integral a) =>
  State (SV.Vector n (SV.Vector (m + 1) a)) (SV.Vector m (Maybe (Finite n)))
stepFullGauss = genVec . M.fromList . map swap <$> go (0, 0)
  where
    go ij = do
      (nextIJ, foundPivot) <- stepGauss ij
      let addMe
            | foundPivot = (ij :)
            | otherwise = id
      addMe . fromMaybe [] <$> traverse go nextIJ
    genVec mp = SV.generate (`M.lookup` mp)

-- | Zero out all elements that are not pivots or free variables, and make
-- pivots non-negative
reduceBack ::
  forall n m a. (KnownNat n, KnownNat m, Integral a) => State (SV.Vector n (SV.Vector (m + 1) a)) ()
reduceBack = do
  modify $ fmap reduceGCD
  for_ (tails (reverse finites)) \case
    [] -> pure ()
    i : js -> do
      eliminator <- gets (`SV.index` i)
      for_ (SV.findIndex (/= 0) (SV.take @m eliminator)) \pivot -> do
        let pVal = eliminator `SV.index` weaken pivot
        for_ js \j -> do
          row <- gets (`SV.index` j)
          let elimVal = row `SV.index` weaken pivot
          modify $ \mat ->
            mat SV.// [(j, reduceGCD $ SV.zipWith (\x y -> pVal * y - elimVal * x) eliminator row)]

-- | Assumes 'reduceBack': all items are zero except for pivots and free
-- variables
withFrees ::
  forall n m a r.
  (KnownNat m, Integral a) =>
  SV.Vector n (SV.Vector (m + 1) a) ->
  ( forall q.
    KnownNat q =>
    [(a, SV.Vector (q + 1) a)] ->
    (a, SV.Vector (q + 1) a) ->
    r
  ) ->
  r
withFrees augMat f = SV.withSizedList freeVars $ \(freeVarsVec :: SV.Vector q (Finite m)) ->
  let ineqs :: [(a, SV.Vector (q + 1) a)]
      ineqs =
        [ (pivot, v')
        | v <- toList augMat
        , pivot <- take 1 . filter (/= 0) $ toList v
        , let v' = SV.generate \j ->
                case strengthen j of
                  Nothing -> SV.last v
                  Just j' -> negate $ v `SV.index` weaken (freeVarsVec `SV.index` j')
        ]
      bigLCM = foldr1 lcm $ fst <$> ineqs
      go (pVal, coeffs) acc = acc + ((* multiple) <$> coeffs)
        where
          multiple = bigLCM `div` pVal
      objective :: SV.Vector (q + 1) a
      objective = foldr go (SV.generate $ maybe 0 (const bigLCM) . strengthen) ineqs
   in f ineqs (bigLCM, objective)
  where
    pivots :: [Finite m]
    pivots = mapMaybe (SV.findIndex (/= 0) . SV.take @m) (toList augMat)
    freeVars :: [Finite m]
    freeVars = S.toList $ S.fromAscList finites `S.difference` S.fromList pivots

-- | Bounds for each of the non-negative variables so that the equation can be
-- non-negative. Usually
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
      | coeff < 0 =
          let ub = constTerm `div` (-coeff)
           in if ub < 0 then IV.empty else Finite 0 <=..<= Finite ub
      | constTerm < 0 = IV.empty
      | otherwise = Finite 0 <=..< PosInf

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

-- | Enumerate all points in the feasible region in depth first search
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
enumIntervalClamped maxSearch iv =
  case (lowerI, upperI) of
    (Just lo, Just hi) | lo <= hi -> [lo .. hi]
    _ -> []
  where
    lowerI = case IV.lowerBound' iv of
      (NegInf, _) -> Just 0
      (Finite a, Closed) -> Just (max 0 a)
      (Finite a, Open) -> Just (max 0 (a + 1))
      (PosInf, _) -> Nothing
    upperI = case IV.upperBound' iv of
      (PosInf, _) -> Just maxSearch
      (Finite b, Closed) -> Just (min maxSearch b)
      (Finite b, Open) -> Just (min maxSearch (b - 1))
      (NegInf, _) -> Nothing
