-- |
-- Module      : AOC2024.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day21 (
  day21a,
  day21b,
)
where

import AOC.Common (digitToIntSafe)
import AOC.Common.Point (Dir (..), Point, V2 (V2), dirPoint)
import AOC.Common.Search (bfsActions)
import AOC.Solver (noFail, type (:~>) (..))
import Control.Applicative (Alternative (empty))
import Control.Monad (guard, mfilter, zipWithM, (<=<))
import Data.Char (intToDigit, isDigit)
import Data.Finite (Finite, finites)
import Data.Foldable (Foldable (toList))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as S
import Safe.Foldable (minimumMay)

type NumPad = Maybe (Finite 10)
type DirPad = Maybe Dir

numPad :: Map Point NumPad
numPad =
  M.fromList
    [ (V2 1 0, Just 0)
    , (V2 2 0, Nothing)
    , (V2 0 1, Just 1)
    , (V2 1 1, Just 2)
    , (V2 2 1, Just 3)
    , (V2 0 2, Just 4)
    , (V2 1 2, Just 5)
    , (V2 2 2, Just 6)
    , (V2 0 3, Just 7)
    , (V2 1 3, Just 8)
    , (V2 2 3, Just 9)
    ]

dirPad :: Map Point DirPad
dirPad =
  M.fromList
    [ (V2 0 0, Just West)
    , (V2 1 0, Just South)
    , (V2 2 0, Just East)
    , (V2 1 1, Just North)
    , (V2 2 1, Nothing)
    ]

class Ord a => Pushable a where
  allPushable :: [a]
  pushLayout :: Map Point (Maybe a)

allPushable' :: Pushable a => [Maybe a]
allPushable' = Nothing : fmap Just allPushable

applyPush :: forall a. Pushable a => Maybe Dir -> Maybe a -> Maybe (Maybe a, Maybe (Maybe a))
applyPush = \case
  Nothing -> \x -> Just (x, Just x)
  Just d -> \x -> do
    y <- M.lookup d =<< M.lookup x pushMap
    pure (y, Nothing)
  where
    pushMap :: Map (Maybe a) (Map Dir (Maybe a))
    pushMap =
      M.fromList
        [ (x, M.fromList [(d, y) | d <- [North ..], y <- maybeToList $ M.lookup (p + dirPoint d) pushLayout])
        | (p, x) <- M.toList pushLayout
        ]

instance Pushable Dir where
  allPushable = [East, South, North, West]
  pushLayout = dirPad

instance Pushable (Finite 10) where
  allPushable = finites
  pushLayout = numPad

-- | Best way to get from button to button. penalize motion
dirPath :: forall a. Pushable a => Map (Maybe a) (Map (Maybe a) [DirPad])
dirPath = M.fromSet ((`M.fromSet` S.fromList allPushable') . go) (S.fromList allPushable')
  where
    go :: Maybe a -> Maybe a -> [DirPad]
    go x y = runPath Nothing . fromJust $ bfsActions step (Left (x, Nothing)) (== Right y)
      where
        step (Left (b, d)) =
          reverse
            [ ( push
              , case bout of
                  Nothing -> Left (b', d')
                  Just o -> Right o
              )
            | push <- allPushable'
            , (d', dout) <- maybeToList $ applyPush push d
            , (b', bout) <- case dout of
                Nothing -> pure (b, Nothing)
                Just push' -> maybeToList $ applyPush push' b
            ]
        step (Right _) = []

dirPathCosts :: Pushable a => Map (Maybe a) (Map (Maybe a) Int)
dirPathCosts = (fmap . fmap) length dirPath

spellDirPathLengths ::
  Ord a =>
  Map (Maybe a) (Map (Maybe a) Int) ->
  [Maybe a] ->
  Int
spellDirPathLengths mp xs = sum $ zipWith (\x y -> (mp M.! x) M.! y) xs (drop 1 xs)

composeDirPathLengths ::
  Ord b =>
  Map (Maybe b) (Map (Maybe b) Int) ->
  Map (Maybe a) (Map (Maybe a) [Maybe b]) ->
  Map (Maybe a) (Map (Maybe a) Int)
composeDirPathLengths mp = (fmap . fmap) (spellDirPathLengths mp . (Nothing :))

runPath :: Pushable a => Maybe a -> [DirPad] -> [Maybe a]
runPath x = \case
  [] -> []
  d : ds -> case applyPush d x of
    Nothing -> error $ "hm..." ++ show d
    Just (y, out) -> maybe id (:) out $ runPath y ds

-- | this seems to work for the answers but not for the sample data
_solveCodeNoSearch :: Int -> [NumPad] -> Int
_solveCodeNoSearch n = spellDirPathLengths mp . (Nothing :)
  where
    mpChain :: [Map DirPad (Map DirPad Int)]
    mpChain = iterate (`composeDirPathLengths` dirPath @Dir) (dirPathCosts @Dir)
    mp = (mpChain !! (n - 1)) `composeDirPathLengths` dirPath @(Finite 10)

solveCodeWithSearch :: Int -> [NumPad] -> Int
solveCodeWithSearch n ps = minimum do
  npp <- toList $ fullPadPaths (Nothing : ps)
  pure $ spellDirPathLengths mp (Nothing : npp)
  where
    mpChain :: [Map DirPad (Map DirPad Int)]
    mpChain = iterate (`composeDirPathLengths` dirPath @Dir) (dirPathCosts @Dir)
    mp = mpChain !! (n - 1)

-- | a lot of these can be pruned waay by getting rid of NEN/ENE etc.
padPaths :: Pushable a => Maybe a -> Maybe a -> Set [DirPad]
padPaths start goal = fromMaybe S.empty do
  minLen <- minimumMay $ length <$> options
  pure $ S.fromList $ filter ((== minLen) . length) options
  where
    options = go S.empty start
    go seen p = do
      guard $ p `S.notMember` seen
      d <- allPushable'
      (p', o) <- maybeToList $ applyPush d p
      (d :) <$> case o of
        Nothing -> go (S.insert p seen) p'
        Just o' -> if o' == goal then pure [] else empty

fullPadPaths :: Pushable a => [Maybe a] -> Set [DirPad]
fullPadPaths xs = S.fromList $ concat <$> zipWithM (\a b -> toList $ padPaths a b) xs (drop 1 xs)

pc :: Char -> Maybe (Finite 10)
pc = fmap fromIntegral . digitToIntSafe <=< mfilter isDigit . Just

day21 :: Int -> [[NumPad]] :~> Int
day21 n =
  MkSol
    { sParse = Just . map (map pc) . lines
    , sShow = show
    , sSolve =
        noFail $
          sum . map solve
    }
  where
    solve p = num * solveCodeWithSearch n p
      where
        num = read (map intToDigit (mapMaybe (fmap fromIntegral) p :: [Int]))

day21a :: [[NumPad]] :~> Int
day21a = day21 2

day21b :: [[NumPad]] :~> Int
day21b = day21 25
