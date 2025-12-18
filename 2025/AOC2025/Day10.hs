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

import AOC.Common ()
import AOC.Solver ((:~>) (..))
import Control.Monad (filterM, guard, (<=<))
import Control.Monad.Trans.State (State, StateT (..), execStateT, runState, state)
import Data.Finite (Finite, finites, shift, strengthen, weaken)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)
import qualified Data.Vector.Sized as SV
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
          backsolved = backsolve (fromIntegral $ maximum t) rowEchelon pivots
       in fmap fst . minimumMay $ (\x -> (sum x, x)) <$> backsolved

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

stepFullGauss ::
  (KnownNat n, KnownNat m, Num a, Eq a) =>
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
