{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
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
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.
module AOC2025.Day10 (
  day10a,
  day10b,
)
where

import AOC.Prelude
import Data.Finite
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntMap.NonEmpty as NEIM
import qualified Data.IntSet as IS
import qualified Data.IntSet.NonEmpty as NEIS
import qualified Data.List.NonEmpty as NE
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map as M
import qualified Data.Map.NonEmpty as NEM
import qualified Data.OrdPSQ as PSQ
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import GHC.TypeNats
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

day10a :: _ :~> _
day10a =
  MkSol
    { sParse =
        noFail $
          map parseMe . lines
    , sShow = show
    , sSolve =
        noFail $ sum . map go . take 1 . drop 2
    }
  where
    parseMe :: String -> ([Bool], [[Int]], [Int])
    parseMe ('[' : xs) =
      ( map (== '#') a
      , map read . splitOn "," . init . tail <$> init ps
      , map read . splitOn "," . init . tail $ last ps
      )
      where
        (a, ']' : bs) = span (/= ']') xs
        ps = words bs
    go :: ([Bool], [[Int]], [Int]) -> Int
    go (targ, buttons, _) =
      minimum
        [ length onButts
        | onButts <- filterM (\_ -> [False, True]) buttons
        , foldr symmetricDifference S.empty (S.fromList <$> onButts) == targSet
        ]
      where
        targSet = S.fromList $ map fst $ filter snd $ zip [0 ..] targ

-- traceShow vecs 3
-- where
--   vecs = flip map buttons \ixes -> zipWith (\i _ -> i `S.member` S.fromAscList ixes) [0..] targ

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference x y = (x <> y) `S.difference` (x `S.intersection` y)

-- [.##.]
-- (...#)
-- (.#.#)
-- (..#.)
-- (..##)
-- (#.#.)
-- (##..)
-- {3,5,4,7}
--
-- ....## a   .
-- .#...# b = #
-- ..###. c   #
-- ##.#.. d   .
--        e
--        f
--
--         e+f = 0 mod 2
--   b+      f = 1 mod 2
--     c+d+e   = 1 mod 2
-- a+b+  d     = 0 mod 2

-- [.##.] (...#) (.#.#) (..#.) (..##) (#.#.) (##..) {3,5,4,7}

-- [.##.] (3)    (1,3) (2)     (2,3)  (0,2)  (0,1) {3,5,4,7}
-- [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
-- [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

day10b :: _ :~> _
day10b =
  MkSol
    { sParse = sParse day10a
    , sShow = show
    , sSolve = Just
    -- fmap sum . traverse (traceShowId . go)
    -- noFail $ sum . map go
    }
  where
    go (_, buttons, targ) = withSizedButtons buttons targ \b t ->
      let (pivots, rowEchelon) = runState stepFullGauss $ toAugMat b t
          -- in rowEchelon
          backsolved = backsolve' (maximum t) rowEchelon pivots
       in minimumMay $ sum <$> backsolved

-- backsolve :: forall n m. (KnownNat n, KnownNat m) => SV.Vector n (SV.Vector (m + 1) Int) -> [SV.Vector m (Maybe Int)]
-- toAugMat b t

-- go :: ([Bool], [[Int]], [Int]) -> Int
-- go (_, buttons, targ) = minimum $ flip evalStateT (0 <$ targMap) $ do
--       tots <- traverse (goo . IS.fromList) buttons
--       -- tots <- traverse (goo . IS.fromList) $ sortOn (Down . length) buttons
--       guard =<< gets (== targMap)
--       traceM (show tots)
--       pure $ sum tots
--   where
--     goo :: IntSet -> StateT (IntMap Int) [] Int
--     goo button = StateT $ \soFar -> traceShowId $
--       takeWhile (and . IM.intersectionWith (>=) targMap . snd) $
--           iterate (\(!i, mp) -> (i + 1, IM.unionWith (+) buttonMap mp)) (0, soFar)
--       -- let leftOver = IM.intersectionWith (-) targMap soFar
--       where
--         buttonMap = IM.fromSet (const 1) button
--     targMap = IM.fromList $ zip [0..] targ

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
  (KnownNat n, KnownNat m) =>
  SV.Vector m (Set (Finite n)) ->
  SV.Vector n Int ->
  SV.Vector n (SV.Vector (m + 1) Int)
toAugMat buttons target = SV.generate \i -> SV.generate \j ->
  case strengthen j of
    Nothing -> target `SV.index` i
    Just j'
      | i `S.member` (buttons `SV.index` j') -> 1
      | otherwise -> 0

-- data StepState n m = StepState
--     { ssRow :: Finite n
--     , ssCol :: Finite m
--     , ssMat :: SV.Vector n (SV.Vector (m + 1) Int)
--     }

nextFin :: KnownNat n => Finite n -> Maybe (Finite n)
nextFin = strengthen . shift

-- type AugMat n m = SV.Vector n (SV.Vector (m + 1) Int)

-- | Assumes everything left of the index is already upper triangular/row
-- echelon. Returns whether or not we found a pivot here
stepGauss ::
  forall n m.
  (KnownNat n, KnownNat m) =>
  (Finite n, Finite m) ->
  State (SV.Vector n (SV.Vector (m + 1) Int)) (Maybe (Finite n, Finite m), Bool)
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
  (KnownNat n, KnownNat m) =>
  State (SV.Vector n (SV.Vector (m + 1) Int)) (SV.Vector m (Maybe (Finite n)))
stepFullGauss = genVec . M.fromList . map swap <$> go (0, 0)
  where
    go ij = do
      (nextIJ, foundPivot) <- stepGauss ij
      let addMe
            | foundPivot = (ij :)
            | otherwise = id
      addMe . fromMaybe [] <$> traverse go nextIJ
    genVec mp = SV.generate (`M.lookup` mp)

backsolve ::
  forall n m. (KnownNat n, KnownNat m) => SV.Vector n (SV.Vector (m + 1) Int) -> [SV.Vector m Int]
backsolve augMat =
  mapMaybe sequenceA $
    execStateT (traverse go (reverse finites)) (pure Nothing :: SV.Vector m (Maybe Int))
  where
    coeffs :: SV.Vector n (SV.Vector m Int)
    coeffs = SV.take @m <$> augMat
    targs :: SV.Vector n Int
    targs = SV.last <$> augMat
    go :: Finite n -> StateT (SV.Vector m (Maybe Int)) [] ()
    go i = StateT $ map ((),) . stepBacksolve (coeffs `SV.index` i) (targs `SV.index` i)

stepBacksolve :: SV.Vector m Int -> Int -> SV.Vector m (Maybe Int) -> [SV.Vector m (Maybe Int)]
stepBacksolve coeffs targ known = evalStateT (SV.zipWithM go coeffs known <* exactlyEqual) (targ, False)
  where
    exactlyEqual = guard . (== 0) . fst =<< get
    go :: Int -> Maybe Int -> StateT (Int, Bool) [] (Maybe Int)
    go c (Just x) = do
      modify $ first $ subtract (c * x)
      pure $ Just x
    go 0 Nothing = get >>= \(_, pastPivot) -> if pastPivot then Just <$> lift [0 .. 180] else pure Nothing
    go c Nothing = do
      (leftover, _) <- get
      -- hm this needs to go backwards, if c is negative. leftovers can start
      x' <- lift [0 .. 180]
      -- x' <- lift [0 .. (leftover `div` c)]
      put (leftover - c * x', True)
      pure (Just x')

-- coeffs = coeffss `SV.index` rowNum
-- targ = targs `SV.index` rowNum
--
-- Vector [Vector [1,1,1,0,10],Vector [0,-1,0,1,1],Vector [0,0,1,0,5],Vector [0,0,0,0,0],Vector [0,0,0,0,0],Vector [0,0,0,0,0]]
-- [Vector [Nothing,Nothing,Nothing,Nothing]]
-- [Vector [Nothing,Nothing,Nothing,Nothing]]
-- [Vector [Nothing,Nothing,Nothing,Nothing]]
-- [Vector [Nothing,Nothing,Just 5,Just 0]]
-- []

backsolve' ::
  forall n m.
  (KnownNat n, KnownNat m) =>
  Int -> SV.Vector n (SV.Vector (m + 1) Int) -> SV.Vector m (Maybe (Finite n)) -> [SV.Vector m Int]
backsolve' maxSearch augMat pivots = mapMaybe (execStateT (traverse go (reverse finites))) starting
  where
    starting = traverse (\case Nothing -> [0 .. maxSearch]; Just _ -> [0]) pivots
    coeffs :: SV.Vector n (SV.Vector m Int)
    coeffs = SV.take @m <$> augMat
    targs :: SV.Vector n Int
    targs = SV.last <$> augMat
    go :: Finite n -> StateT (SV.Vector m Int) Maybe ()
    go i = StateT $ fmap ((),) . stepBacksolve' (coeffs `SV.index` i) (targs `SV.index` i)

stepBacksolve' :: SV.Vector m Int -> Int -> SV.Vector m Int -> Maybe (SV.Vector m Int)
stepBacksolve' coeffs targ known = case SV.findIndex (/= 0) coeffs of
  Nothing -> pure known
  Just pivot -> do
    let goal = targ - sum (SV.zipWith (*) coeffs known)
        (x, residue) = goal `divMod` (coeffs `SV.index` pivot)
    -- traceM $ show (coeffs, targ, known, pivot, goal, x, residue, residue == 0 && x >= 0)
    guard $ residue == 0
    guard $ x >= 0
    pure $ known SV.// [(pivot, x)]

-- stepBacksolve :: SV.Vector m Int -> Int -> SV.Vector m (Maybe Int) -> SV.Vector m (Maybe Int)
-- stepBacksolve coeffs targ known = evalState (SV.zipWithM go coeffs known) (targ, False)
--   where
--     exactlyEqual = guard . (== 0) . fst =<< get
--     go :: Int -> Maybe Int -> StateT (Int, Bool) [] (Maybe Int)
--     go c (Just x) = do
--         modify $ first $ subtract (c * x)
--         pure $ Just x
--     go 0 Nothing = get >>= \(_, pastPivot) -> if pastPivot then Just <$> lift [0..180] else pure Nothing
--     go c Nothing = do
--       (leftover,_) <- get
--       -- hm this needs to go backwards, if c is negative. leftovers can start
--       x' <- lift [0 .. 180]
--       -- x' <- lift [0 .. (leftover `div` c)]
--       put (leftover - c * x', True)
--       pure (Just x')

-- mapMaybe sequenceA $ execStateT (traverse go (reverse finites)) (pure Nothing :: SV.Vector m (Maybe Int))
-- where
--   coeffs :: SV.Vector n (SV.Vector m Int)
--   coeffs = SV.take @m <$> augMat
--   targs :: SV.Vector n Int
--   targs = SV.last <$> augMat
--   go :: Finite n -> StateT (SV.Vector m (Maybe Int)) [] ()
--   go i = StateT $ map ((),) . stepBacksolve (coeffs `SV.index` i) (targs `SV.index` i)

-- stepGuass (StepState i j mat)
--     | j == maxBound = Just $ StepState i j mat
--     | otherwise = case pivotIx of
--       Nothing -> flip (StepState i) mat <$> nextFin j
--       Just p ->
--       -- pivotIx :: Finite n <- SV.findIndex (\(k, row) -> k >= i && (row `SV.index` weaken j) /= 0) $ SV.indexed mat
--         let pivotRow = mat `SV.index` p
--             mat' = mat SV.// [(i, pivotRow), (p, origRow)]
--             pVal = pivotRow `SV.index` weaken j
--             mat'' = SV.indexed mat' <&> \(k, row) ->
--                      let elimVal = row `SV.index` weaken j
--                      in if k <= i
--                          then row
--                          else SV.zipWith _ pivotRow row
--         in do i' <-
--             nextFin j <&> \j' ->
--               StepState (i + 1) j'
--   where
--     origRow = mat `SV.index` i
--     pivotIx = SV.findIndex (\(k, row) -> k >= i && (row `SV.index` weaken j) /= 0) $ SV.indexed mat

-- col :: SV.Vector n Int
-- col = (`SV.index` weaken i) mat
-- pivotIx = firstJust

-- (...#)
-- (.#.#)
-- (..#.)
-- (..##)
-- (#.#.)
-- (##..)
-- {3547}

--         e+f = 3
--   b+      f = 5
--     c+d+e   = 4
-- a+b+  d     = 7

-- ....## a   3
-- .#...# b = 5
-- ..###. c   4
-- ##.#.. d   7
--        e
--        f

-- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
--
-- 0 0 0 0 1 1 | 3
-- 0 1 0 0 0 1 | 5
-- 0 0 1 1 1 0 | 4
-- 1 1 0 1 0 0 | 7

-- 0 0 0 0 0 1  1 | 3
-- 0 1 0 0 0 0  1 | 5
-- 0 0 1 1 1 0  0 | 4
-- 1 0 0 1 0 0 -1 | 2

-- 1 0 0 1 0 0 -1 | 2
-- 0 1 0 0 0 0  1 | 5
-- 0 0 1 1 1 0  0 | 4
-- 0 0 0 0 0 1  1 | 3

-- [..#...#.] (0,1,4,5,6,7) (0,2,3) (0,1,2,3,5,6,7) (0,1,2,3,6) (1,4) (0,1,2,5,6,7) {215,215,28,19,198,204,204,204}
--
--
-- 0   1 1 1 1 0 1 215
-- 1   1 0 1 1 1 1 215
-- 2   0 1 1 1 0 1  28
-- 3   0 1 1 1 0 0  19
-- 4   1 0 0 0 1 0 198
-- 5   1 0 1 0 0 1 204
-- 6   1 0 1 1 0 1 204
-- 7   1 0 1 0 0 1 204

-- 0   1  1  1  1  1  0  1  215
-- 1   0 -1 -1  0  0  1  0    0
-- 2   0  1  1  1  1  0  1   28
-- 3   0  1  1  1  1  0  0   19
-- 4   0 -1 -1 -1 -1  1 -1  -17
-- 5   0 -1 -1  0 -1  0  0  -11
-- 6   0 -1 -1  0  0  0  0  -11
-- 7   0 -1 -1  0 -1  0  0  -11

-- 0   1  1  1  1  1  0  1  215
-- 1   0 -1 -1  0  0  1  0    0
-- 2   0  0  0  1  1  1  1   28
-- 3   0  1  1  1  1  0  0   19
-- 4   0 -1 -1 -1 -1  1 -1  -17
-- 5   0 -1 -1  0 -1  0  0  -11
-- 6   0 -1 -1  0  0  0  0  -11
-- 7   0 -1 -1  0 -1  0  0  -11

-- 0   1  1  1  1  1  0  1  215
-- 1   0 -1 -1  0  0  1  0    0
-- 2   0  0  0  1  1  1  1   28
-- 3   0  0  0  1  1  1  0   19
-- 4   0  0  0 -1 -1  0 -1  -17
-- 5   0  0  0  0 -1 -1  0  -11
-- 6   0  0  0  0  0 -1  0  -11
-- 7   0  0  0  0 -1 -1  0  -11

-- 0   1  1  1  1  1  0  1  215
-- 1   0 -1 -1  0  0  1  0    0
-- 2   0  0  0  1  1  1  1   28
-- 3   0  0  0  0  0  0 -1   -9
-- 4   0  0  0  0  0  1  0   12
-- 5   0  0  0  0 -1 -1  0  -11
-- 6   0  0  0  0  0 -1  0  -11
-- 7   0  0  0  0 -1 -1  0  -11

-- 0   1  1  1  1  1  0  1  215
-- 1   0 -1 -1  0  0  1  0    0
-- 2   0  0  0  1  1  1  1   28
-- 3   0  0  0  0  0  0 -1   -9
-- 4   0  0  0  0  0  1  0   12
-- 5   0  0  0  0 -1 -1  0  -11
-- 6   0  0  0  0  0 -1  0  -11
-- 7   0  0  0  0  0  0  0    0

-- 0   1  1  1  1  1  0  1  215
-- 1   0 -1 -1  0  0  1  0    0
-- 2   0  0  0  1  1  1  1   28
-- 3   0  0  0  0  0  0 -1   -9
-- 4   0  0  0  0  0  1  0   12
-- 5   0  0  0  0 -1 -1  0  -11
-- 6   0  0  0  0  0  0  0    1
-- 7   0  0  0  0  0  0  0    0

-- 0   1  1  1  1  1  0  1  215
-- 1   0 -1 -1  0  0  1  0    0
-- 2   0  0  0  1  1  1  1   28
-- 5   0  0  0  0 -1 -1  0  -11
-- 4   0  0  0  0  0  1  0   12
-- 3   0  0  0  0  0  0 -1   -9
-- 6   0  0  0  0  0  0  0    1
-- 7   0  0  0  0  0  0  0    0
