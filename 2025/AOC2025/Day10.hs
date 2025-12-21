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
import Data.Finite (Finite, finites, shift, strengthen, weaken)
import Data.Foldable
import Data.Foldable1 hiding (head, foldr1, maximum)
import Data.Functor ((<&>))
import Data.List (tails)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)
import Data.Bifunctor
import qualified Data.Vector.Sized as SV
import Debug.Trace
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
      let (pivots, rowEchelon) = runState stepFullGauss $ toAugMat b (fromIntegral <$> t)
          reduced = execState reduceBack rowEchelon
          !(!constrs, !obj) = toEqns reduced
          !candidates = allCandidates (fromIntegral $ maximum t) constrs
      in minimumMay $ map (\coeffs -> dotMap (M.insert Nothing 1 $ M.mapKeysMonotonic Just coeffs) obj) candidates
          -- backsolved = backsolve (fromIntegral $ maximum t) rowEchelon pivots
       -- in fmap fst . minimumMay $ (\x -> (sum x, x)) <$> backsolved

-- [(Nothing,-18),(Just (finite 8),1),(Just (finite 9),1)]
-- [(Nothing,11),(Just (finite 8),1),(Just (finite 9),-1)]
-- [(Nothing,87),(Just (finite 8),-3),(Just (finite 9),-2)]
-- [(Nothing,7),(Just (finite 8),1)]
-- [(Nothing,-8),(Just (finite 9),1)]
-- [(Nothing,80),(Just (finite 8),-2),(Just (finite 9),-3)]
-- [(Nothing,72),(Just (finite 8),-3),(Just (finite 9),-2)]
-- [(Nothing,-69),(Just (finite 8),3),(Just (finite 9),3)]
--
-- 0 <= -18 +   x_8 +   x_9
-- 0 <=  11 +   x_8 -   x_9
-- 0 <=  87 - 3 x_8 - 2 x_9
-- 0 <=   7 +   x_8
-- 0 <=  -8 +           x_9
-- 0 <=  80 - 2 x_8 - 3 x_9
-- 0 <=  72 - 3 x_8 - 2 x_9
-- 0 <= -69 + 3 x_8 + 3 x_9
--
-- objective: 162 - 8 x_8 - 9 x_9
--
--  18 <=    x_8 +   x_9
-- -11 <=    x_8 -   x_9
-- -87 <= -3 x_8 - 2 x_9
--  -7 <=    x_8
--   8 <=            x_9
-- -80 <= -2 x_8 - 3 x_9
-- -72 <= -3 x_8 - 2 x_9
--  69 <=  3 x_8 + 3 x_9

-- fromList [(Nothing,162),(Just (finite 8),-2),(Just (finite 9),-3)]
--
-- 162 - 2 x_8 - 3 x_9


-- the hard case:
-- ([False,True,False,True,True,False,True,True,False,True],[[1,4,6,7],[0,1,3,5,7,9],[3,9],[0,1,2,3,5,8],[2,3,4,6,8,9],[1,3,4,5,6,7,9],[0,1,2,5,6,7],[1,7],[2,3,4,9],[0],[1,2,4,7],[0,2,4,6,7,8],[1,2,3,5,8]],[159,212,79,188,77,173,55,192,53,157])
--
-- (Vector [Just (finite 0),Just (finite 1),Just (finite 2),Just (finite 3),Just
-- (finite 4),Just (finite 5),Just (finite 6),Just (finite 7),Just (finite
-- 8),Nothing,Nothing,Just (finite 9),Nothing],Vector [Vector
-- [1,1,0,1,0,1,1,1,0,0,1,0,1,212],Vector [0,1,0,1,0,0,1,0,0,1,0,1,0,159],Vector
-- [0,0,1,0,1,1,-1,0,1,-1,0,-1,1,29],Vector [0,0,0,1,1,0,1,0,1,0,1,1,1,79],Vector
-- [0,0,0,0,1,0,0,-1,1,1,0,2,-1,24],Vector
-- [0,0,0,0,0,1,0,0,0,-1,0,-1,1,14],Vector
-- [0,0,0,0,0,0,1,0,-1,0,-1,0,0,-22],Vector
-- [0,0,0,0,0,0,0,1,1,-1,2,0,1,57],Vector
-- [0,0,0,0,0,0,0,0,-2,0,-2,0,0,-48],Vector [0,0,0,0,0,0,0,0,0,0,0,2,0,22]])
--
--    0  1  2  3  4  5  6  7  8  9 10 11 12
-- 0: 1  1  0  1  0  1  1  1  0  0  1  0  1  212
-- 1: 0  1  0  1  0  0  1  0  0  1  0  1  0  159
-- 2: 0  0  1  0  1  1 -1  0  1 -1  0 -1  1   29
-- 3: 0  0  0  1  1  0  1  0  1  0  1  1  1   79
-- 4: 0  0  0  0  1  0  0 -1  1  1  0  2 -1   24
-- 5: 0  0  0  0  0  1  0  0  0 -1  0 -1  1   14
-- 6: 0  0  0  0  0  0  1  0 -1  0 -1  0  0  -22
-- 7: 0  0  0  0  0  0  0  1  1 -1  2  0  1   57
-- 8: 0  0  0  0  0  0  0  0 -2  0 -2  0  0  -48
-- 9: 0  0  0  0  0  0  0  0  0  0  0  2  0   22
--
-- step 1:
-- - look at row 9 ->
--   - pivot: x_11, free: x_12
--     -> subtract out other variables from target (none, so = 22 still)
--     -> iterate over [0..maxBound] for x_12
--        -> for each of these, put the appropriate x_11 if possible
--   - pivot: x_8, free: x_9, x_10
--     -> subtract out other variables from target (-48 - 0 = -48)
--     -> iterate over [0..maxBound] for x_9
--       -> iterate over [0 .. maxBound - whatever is leftover for x_10]
--         -> for each of these, put the appropriate x_8
--
-- ok but how do we make sure we can cap the iteration for x_10? in practice,
-- we are solving -2 x_8 - 2 x_10 = -48 => x_8 = 24 - x_10, so actually it is
-- only possible for x_10 to go from 0 to 24. if we iterate over x_9 first and
-- then x_10 then that doesn't help that much, since we have to cut out 200
-- points 200 times. but if we iterate over x_10 first then that is much
-- better... we can iterate based on the size of the coefficient. or well,
-- this really only matters if the free variable is the same sign as the
-- pivot. if it was x = 24 + y then that doesn't help us cut anything at all.
-- but also actually maybe we want to see if it really _is_ x = 24 + y then
-- maybe we can assume y = 0? idk
--
-- okay no this is bad. this won't work.
--
-- One probably would be to solve it out to a Map (Maybe FreeVar) Coefficient
-- by just reducing them all then we minimize the map number.
--
-- We can simplex by finding the intersections of all the planes...or it's
-- probably small enough to directly brute force because we have the direct
-- equation a*x - b*y + 3, we can do the descending triangular one.

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
  (KnownNat n, KnownNat m, Num a, Eq a, Integral a) =>
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

reduceGCD xs
  | null xs' = xs
  | otherwise = (* firstSign) . (`div` foldr1 gcd xs') <$> xs
  where
    xs' = filter (/= 0) $ toList xs
    firstSign = signum $ head xs'

stepFullGauss ::
  (KnownNat n, KnownNat m, Num a, Eq a, Integral a) =>
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

-- traverse go (reverse finites)
-- where
--   go :: Finite n -> State (SV.Vector n (SV.Vector (m + 1) a)) ()
--   go i = traverse go' finites
--     where

-- -- | a x + c_0 x_0 + .. = d
-- --
-- -- x is non-free, x_0, x_1 .. are free variables
-- data OnFree m = OnFree
--   { ofCoeff :: Integer   -- ^ a
--   , ofFrees :: Map (Finite m) Integer   -- ^ c_i
--   , ofSum :: Integer  -- ^ d
--   }

type OnFree m = Map (Maybe (Finite m)) Integer

--    1  0  0  0  0  0  0  0  0  1  0  0 -1    6
--    0  1  0  0  0  0  0  0  0  1  0  0 -1  115
--    0  0  1  0  0  0  0  0  0  0 -1  0  0  -18
--    0  0  0  1  0  0  0  0  0  0  0  0  1   31
--    0  0  0  0  1  0  0  0  0  0  0  0  0   11
--    0  0  0  0  0  1  0  0  0 -1  0  0  1   25
--    0  0  0  0  0  0  1  0  0  0  0  0  0    2
--    0  0  0  0  0  0  0 -1  0  1 -1  0 -1  -33
--    0  0  0  0  0  0  0  0 -1  0 -1  0  0  -24
--    0  0  0  0  0  0  0  0  0  0  0  1  0   11

-- | return equaities (things that must be positive) and objective function,
-- in terms of free variables
--
-- assumes 'reduceBack' -- all items are zero except for pivots and free
-- variables
toEqns ::
  forall n m. (KnownNat n, KnownNat m) =>
  SV.Vector n (SV.Vector (m + 1) Integer) ->
  ([OnFree m], OnFree m)
toEqns augMat = (snd <$> ineqs, objective)
  where
    -- coeffs :: SV.Vector n (SV.Vector m Integer)
    -- coeffs = SV.take @m <$> augMat
    -- targs :: SV.Vector n Integer
    -- targs = SV.last <$> augMat
    ineqs :: [(Integer, OnFree m)]
    ineqs =
      [ (pivot, M.fromList $ (Nothing, targ) : (bimap Just negate <$> rest))
        | v <- SV.toList augMat
      , let coeffs = SV.take @m v
            targ = SV.last v
      , (_, pivot):rest <- [filter ((/= 0) . snd) (toList (SV.indexed coeffs))]
      ]
    bigLCM = foldr1 gcd $ fst <$> ineqs
    objective :: OnFree m
    objective = foldr1 (M.unionWith (+))
        [ (* multiple) <$> coeffs
          | (pVal, coeffs) <- ineqs
        , let multiple = bigLCM `div` pVal
        ]

dotMap :: (Ord k, Num a) => Map k a -> Map k a -> a
dotMap x y = sum $ M.unionWith (*) x y

allCandidates :: forall m. Integer -> [OnFree m] -> [Map (Finite m) Integer]
allCandidates maxSearch constrs = filter isGood . sequence . M.fromSet (const [0 .. maxSearch]) $ allVars
  where
    allVars = foldMap (S.fromList . catMaybes . M.keys) constrs
    isGood :: Map (Finite m) Integer -> Bool
    isGood = (`all` constrs) . isGooder
    isGooder :: Map (Finite m) Integer -> OnFree m -> Bool
    isGooder xs = (>= 0) . dotMap (M.insert Nothing 1 (M.mapKeysMonotonic Just xs))

candRanges :: forall m. [OnFree m] -> Map (Finite m) (IV.Interval)


--    0  1  2  3  4  5  6  7  8  9 10 11 12
-- 0: 1  1  0  1  0  1  1  1  0  0  1  0  1  212
-- 1: 0  1  0  1  0  0  1  0  0  1  0  1  0  159
-- 2: 0  0  1  0  1  1 -1  0  1 -1  0 -1  1   29
-- 3: 0  0  0  1  1  0  1  0  1  0  1  1  1   79
-- 4: 0  0  0  0  1  0  0 -1  1  1  0  2 -1   24
-- 5: 0  0  0  0  0  1  0  0  0 -1  0 -1  1   14
-- 6: 0  0  0  0  0  0  1  0 -1  0 -1  0  0  -22
-- 7: 0  0  0  0  0  0  0  1  1 -1  2  0  1   57
-- 8: 0  0  0  0  0  0  0  0 -2  0 -2  0  0  -48
-- 9: 0  0  0  0  0  0  0  0  0  0  0  2  0   22
--
-- add all:
--
--    1  2  1  3  3  3  3  1  1 -1  1  4  4  526
--
-- imagine we can zero out non-free variables:
--
--    0  1  2  3  4  5  6  7  8  9 10 11 12
-- 0: 1  1  0  1  0  1  1  1  0  0  1  0  1  212
-- 1: 0  1  0  1  0  0  1  0  0  1  0  1  0  159
-- 2: 0  0  1  0  1  1 -1  0  1 -1  0 -1  1   29
-- 3: 0  0  0  1  1  0  1  0  1  0  1  1  1   79
-- 4: 0  0  0  0  1  0  0 -1  1  1  0  2 -1   24
-- 5: 0  0  0  0  0  1  0  0  0 -1  0 -1  1   14
-- 6: 0  0  0  0  0  0  1  0 -1  0 -1  0  0  -22
-- 7: 0  0  0  0  0  0  0  1  1 -1  2  0  1   57  <- multiply by LCM of pivot amounts, subtract
-- 8: 0  0  0  0  0  0  0  0 -2  0 -2  0  0  -48
-- 9: 0  0  0  0  0  0  0  0  0  0  0  2  0   22
--
-- add all:
--    1  2  1  3  3  3  3  1  1 -1  1  4  4  526
--
--    1  0  0  0  0  0  0  0  0  1  0  0 -1    6
--    0  1  0  0  0  0  0  0  0  1  0  0 -1  115
--    0  0  1  0  0  0  0  0  0  0 -1  0  0  -18
--    0  0  0  1  0  0  0  0  0  0  0  0  1   31
--    0  0  0  0  1  0  0  0  0  0  0  0  0   11
--    0  0  0  0  0  1  0  0  0 -1  0  0  1   25
--    0  0  0  0  0  0  1  0  0  0  0  0  0    2
--    0  0  0  0  0  0  0 -1  0  1 -1  0 -1  -33
--    0  0  0  0  0  0  0  0 -1  0 -1  0  0  -24
--    0  0  0  0  0  0  0  0  0  0  0  1  0   11
--
-- add all:
--    1  1  1  1  1  1  1 -1 -1 -1 -3  1 -2  126
-- x_0 + x_1 + x_2 + x_3 + x_4 + x_5 + x_6 - x_7 - x_8 - x_9 - 3 x_10 + x_11 - 2 x_12 = 126
--
--
-- x_0 = -x_9 + x_12 + 6
-- x_1 = -x_9 + x_12 + 115
-- x_2 = x_10 - 18
--
-- 0. make all the pivot values the same (gcd)
-- 1. add up all the free variable contributions, c_0 c_1 c_2
-- 2. add up all the RHS T
-- 3. we are optimizing  T + (1 - c_0) x_0 + (1 - c_1) x_1 + ...
-- 4. quick job could probably brute force search them for feasiable results,
-- but now we can eliminate cases where c_i = 1

-- -- | Reduce to an equation on the n number of free variables, a*x_0 + b*x_1 +
-- -- c
-- objectiveFunc :: forall n m. (KnownNat n, KnownNat m) =>
--   SV.Vector n (SV.Vector (m + 1) Integer) ->
--   SV.Vector m (Maybe (Finite n)) ->
--   Map (Maybe (Finite m)) Integer
-- objectiveFunc augMat pivots = _
--   where
--     freeVars :: Set (Finite m)
--     freeVars = S.fromAscList [ i
--                              | (i, Nothing) <- toList $ SV.indexed pivots
--                              ]
--     coeffs :: SV.Vector n (SV.Vector m Integer)
--     coeffs = SV.take @m <$> augMat
--     targs :: SV.Vector n Integer
--     targs = SV.last <$> augMat

-- | Using a single max search and searching N^V (v number of free variables)
-- is pretty bad
backsolve ::
  forall n m.
  (KnownNat n, KnownNat m) =>
  Integer ->
  SV.Vector n (SV.Vector (m + 1) Integer) ->
  SV.Vector m (Maybe (Finite n)) ->
  [SV.Vector m Integer]
backsolve maxSearch augMat pivots = mapMaybe (execStateT (traverse_ go (reverse finites) >> validate)) starting
  where
    starting = traverse (\case Nothing -> [0 .. maxSearch]; Just _ -> [0]) pivots
    coeffs :: SV.Vector n (SV.Vector m Integer)
    coeffs = SV.take @m <$> augMat
    targs :: SV.Vector n Integer
    targs = SV.last <$> augMat
    go :: Finite n -> StateT (SV.Vector m Integer) Maybe ()
    go i = StateT $ fmap ((),) . stepBacksolve (coeffs `SV.index` i) (targs `SV.index` i)
    validate = pure ()

stepBacksolve ::
  KnownNat m => SV.Vector m Integer -> Integer -> SV.Vector m Integer -> Maybe (SV.Vector m Integer)
stepBacksolve coeffs targ known = case SV.findIndex (/= 0) coeffs of
  Nothing -> pure known
  Just pivot -> do
    let goal = targ - (coeffs `L.dot` known)
        (x, residue) = goal `divMod` (coeffs `SV.index` pivot)
    -- traceM $ show (coeffs, targ, known, pivot, goal, x, residue, residue == 0 && x >= 0)
    guard $ residue == 0
    guard $ x >= 0
    pure $ known SV.// [(pivot, x)]
