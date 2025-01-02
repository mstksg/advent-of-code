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
import AOC.Solver (noFail, type (:~>) (..))
import Control.Applicative (liftA3)
import Control.Monad (mfilter, (<=<))
import Data.Char (intToDigit, isDigit)
import Data.Finite (Finite, finites)
import Data.Functor ((<&>))
import qualified Data.Graph.Inductive as G
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, maybeToList)
import Data.Tuple (swap)

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

buttonGraph ::
  forall a.
  Pushable a =>
  (G.Gr (Either (Maybe a, DirPad, DirPad) (Maybe a)) DirPad, Map (Maybe a) Int, Map (Maybe a) Int)
buttonGraph = (G.mkGraph (swap <$> M.toList nodes) edges, startMap, endMap)
  where
    nodes :: Map (Either (Maybe a, DirPad, DirPad) (Maybe a)) Int
    nodes =
      M.fromList . flip zip [0 ..] $
        (Left <$> liftA3 (,,) allPushable' allPushable' allPushable')
          ++ (Right <$> allPushable')
    startMap = M.fromList [(n, i) | (Left (n, Nothing, Nothing), i) <- M.toList nodes]
    endMap = M.fromList [(n, i) | (Right n, i) <- M.toList nodes]
    edges :: [(Int, Int, DirPad)]
    edges = do
      (Left (b, d, e), node) <- M.toList nodes
      push <- reverse allPushable'
      (e', eout) <- maybeToList $ applyPush push e
      (d', dout) <- case eout of
        Nothing -> pure (d, Nothing)
        Just push' -> maybeToList $ applyPush push' d
      (b', bout) <- case dout of
        Nothing -> pure (b, Nothing)
        Just push' -> maybeToList $ applyPush push' b
      pure case bout of
        Nothing -> (node, nodes M.! Left (b', d', e'), push)
        Just o -> (node, nodes M.! Right o, push)

-- | Best way to get from button to button. penalize motion two bots down
dirPath :: forall a. Pushable a => Map (Maybe a) (Map (Maybe a) [DirPad])
dirPath =
  st <&> \i ->
    en <&> \j ->
      runPath Nothing . runPath Nothing . drop 1 . map snd . G.unLPath $ G.lesp i j bg
  where
    (bg, st, en) = buttonGraph

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

solveCode :: Int -> [NumPad] -> Int
solveCode n = spellDirPathLengths mp . (Nothing :)
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
    solve p = num * solveCode n p
      where
        num = read (map intToDigit (mapMaybe (fmap fromIntegral) p :: [Int]))

day21a :: [[NumPad]] :~> Int
day21a = day21 2

day21b :: [[NumPad]] :~> Int
day21b = day21 25
