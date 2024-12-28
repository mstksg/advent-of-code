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

import AOC.Common (digitToIntSafe, (!!!))
import AOC.Common.Point (Dir (..), Point, V2 (V2), dirPoint)
import AOC.Common.Search (bfsActions)
import AOC.Solver (noFail, type (:~>) (..))
import Control.Monad (mfilter, (<=<))
import Data.Char (intToDigit, isDigit)
import Data.Finite (Finite, finites)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe, maybeToList)
import qualified Data.Set as S

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

class (Ord a, Show a) => Pushable a where
  allPushable :: [a]
  pushMap :: Map (Maybe a) (Map Dir (Maybe a))

allPushable' :: Pushable a => [Maybe a]
allPushable' = Nothing : fmap Just allPushable

pushMapFromLayout :: Pushable a => Map Point (Maybe a) -> Map (Maybe a) (Map Dir (Maybe a))
pushMapFromLayout pushLayout =
  M.fromList
    [ (x, M.fromList [(d, y) | d <- [North ..], y <- maybeToList $ M.lookup (p + dirPoint d) pushLayout])
    | (p, x) <- M.toList pushLayout
    ]

applyPush :: forall a. Pushable a => Maybe Dir -> Maybe a -> Maybe (Maybe a, Maybe (Maybe a))
applyPush = \case
  Nothing -> \x -> Just (x, Just x)
  Just d -> \x -> do
    y <- M.lookup d =<< M.lookup x pushMap
    pure (y, Nothing)

instance Pushable Dir where
  allPushable = [North ..]
  pushMap = pushMapFromLayout dirPad

instance Pushable (Finite 10) where
  allPushable = finites
  pushMap = pushMapFromLayout numPad

-- | Best way to get from button to button. penalize motion two bots down
dirPath :: forall a. Pushable a => Map (Maybe a) (Map (Maybe a) [DirPad])
dirPath = M.fromSet ((`M.fromSet` S.fromList allPushable') . go) (S.fromList allPushable')
  where
    go :: Maybe a -> Maybe a -> [DirPad]
    go x y =
      runPath Nothing . runPath Nothing . fromJust $
        bfsActions step (Left (x, Nothing, Nothing)) (== Right y)
      where
        step (Left (b, d, e)) =
          reverse
            [ ( push
              , case bout of
                  Nothing -> Left (b', d', e')
                  Just o -> Right o
              )
            | push <- allPushable'
            , (e', eout) <- maybeToList $ applyPush push e
            , (d', dout) <- case eout of
                Nothing -> pure (d, Nothing)
                Just push' -> maybeToList $ applyPush push' d
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

dirPathChain :: Int -> Map DirPad (Map DirPad Int)
dirPathChain n = iterate (`composeDirPathLengths` dirPath @Dir) (dirPathCosts @Dir) !!! n

solveCodeNoSearch :: Int -> [NumPad] -> Int
solveCodeNoSearch n = spellDirPathLengths mp . (Nothing :)
  where
    mp = dirPathChain (n - 1) `composeDirPathLengths` dirPath @(Finite 10)

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
    solve p = num * solveCodeNoSearch n p
      where
        num = read (map intToDigit (mapMaybe (fmap fromIntegral) p :: [Int]))

day21a :: [[NumPad]] :~> Int
day21a = day21 2

day21b :: [[NumPad]] :~> Int
day21b = day21 25
