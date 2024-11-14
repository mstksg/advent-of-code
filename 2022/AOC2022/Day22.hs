{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : AOC2022.Day22
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day22 (
  day22a,
  day22b,
)
where

import AOC.Common (listTup)
import AOC.Common.Point (Dir (..), Point, dirPoint, parseAsciiMap, parseDir)
import AOC.Solver ((:~>) (..))
import Control.Applicative (many, (<|>))
import Control.Monad ((<=<))
import Control.Monad.Trans.State (StateT (..), evalStateT)
import Data.Bitraversable (bitraverse)
import Data.Char (isDigit, isSpace)
import Data.Coerce (coerce)
import Data.List (foldl', uncons)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Monoidal as MM
import Data.Semigroup (Max (..), Min (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Strict (T2 (..), sfst, ssnd)
import Linear (V2 (..), (*^))
import Safe (minimumMay)
import Text.Read (readMaybe)

data Tile = Floor | Wall
  deriving stock (Show, Eq, Ord)

data Step
  = Turn Dir
  | Forward Int
  deriving stock (Show, Eq, Ord)

parseStep :: String -> Maybe [Step]
parseStep = evalStateT (many (Turn <$> stepTurn <|> Forward <$> stepForward))
  where
    stepTurn = (South <>) <$> StateT (bitraverse parseDir pure <=< uncons)
    stepForward = StateT (bitraverse readMaybe pure . span isDigit)

parseInput :: String -> Maybe (Int, Map Point Tile, [Step])
parseInput inp = do
  (rawMp, rawSteps) <- listTup $ splitOn "\n\n" inp
  let mp = parseAsciiMap identChar rawMp
  steps <- parseStep rawSteps
  gridSize <- minimumMay $ map (length . filter (not . isSpace)) (lines rawMp)
  pure (gridSize, mp, steps)
  where
    identChar '.' = Just Floor
    identChar '#' = Just Wall
    identChar _ = Nothing

data MoveState = MS {pos :: !Point, dir :: !Dir}
  deriving stock (Show, Eq, Ord)

step ::
  (MoveState -> MoveState) ->
  Map Point Tile ->
  MoveState ->
  Step ->
  MoveState
step singleStepper mp (MS p d) = \case
  Turn e -> MS p (d <> e)
  Forward n -> stepStraight n (MS p d)
  where
    stepStraight 0 ms = ms
    stepStraight !n ms
      | mp M.! q' == Floor = stepStraight (n - 1) nextStep
      | otherwise = ms
      where
        nextStep@(MS q' _) = singleStepper ms

score :: MoveState -> Int
score (MS (V2 x y) d) = 1000 * (y + 1) + 4 * (x + 1) + dp
  where
    dp = case d of
      East -> 0
      South -> 3
      West -> 2
      North -> 1

solve ::
  (MoveState -> MoveState) ->
  Map Point Tile ->
  [Step] ->
  MoveState
solve singleStepper mp = foldl' (step singleStepper mp) s0
  where
    x0 = minimum [x | V2 x y <- M.keys mp, y == 0]
    s0 = MS (V2 x0 0) East

day22 ::
  (Int -> Set Point -> MoveState -> MoveState) ->
  (Int, Map Point Tile, [Step]) :~> MoveState
day22 singleStepper =
  MkSol
    { sParse = parseInput
    , sShow = show . score
    , sSolve = \(gridSize, mp, xs) ->
        Just $
          solve (singleStepper gridSize (M.keysSet mp)) mp xs
    }

day22a :: (Int, Map Point Tile, [Step]) :~> MoveState
day22a = day22 stepper
  where
    stepper _ pts = go
      where
        xCache, yCache :: Map Int (T2 Int Int)
        T2 xCache yCache = coerce $ flip foldMap pts \(V2 x y) ->
          T2
            (MM.singleton x (T2 (Min y) (Max y)))
            (MM.singleton y (T2 (Min x) (Max x)))
        go (MS p d)
          | naiveStep `S.member` pts = MS naiveStep d
          | otherwise = case d of
              North -> MS (V2 x (sfst $ xCache M.! x)) d
              East -> MS (V2 (sfst $ yCache M.! y) y) d
              South -> MS (V2 x (ssnd $ xCache M.! x)) d
              West -> MS (V2 (ssnd $ yCache M.! y) y) d
          where
            naiveStep@(V2 x y) = p + dirPoint d

day22b :: (Int, Map Point Tile, [Step]) :~> MoveState
day22b = day22 stepForwardFrom

stepForwardFrom ::
  -- | Grid size
  Int ->
  -- | Valid points
  Set Point ->
  -- | Origin
  MoveState ->
  -- | End
  MoveState
stepForwardFrom g pts = go 1
  where
    go :: Int -> MoveState -> MoveState
    go n (MS x0 d0)
      | naiveStep `S.member` pts = MS naiveStep d0
      | otherwise =
          let MS x1 d1 = go (turnDist + 1 + overDist) (MS x0 (d0 <> East))
              MS x2 d2 = go (turnDist + 1 + boundDist) (MS x1 (d1 <> West))
           in MS x2 (d2 <> East)
      where
        naiveStep = x0 + n *^ dirPoint d0
        V2 turnDist boundDist = distToBoundary g d0 x0
        V2 _ overDist = distToBoundary g (d0 <> South) naiveStep

-- | Distance to boundary for the rotation algorithm
--
-- x: Distance forward to boundary within the current square
-- y: Distance to the left to boundary within the current square
distToBoundary :: Int -> Dir -> Point -> Point
distToBoundary g = \case
  North -> fmap ((`mod` g) . negate . (+ 1))
  East -> \(V2 x y) -> V2 (y `mod` g) (negate (x + 1) `mod` g)
  West -> \(V2 x y) -> V2 (negate (y + 1) `mod` g) (x `mod` g)
  South -> fmap (`mod` g)

-- ............
-- .......EJ...
-- .......DI...
-- .......CH...
-- .......BG...
-- .......AF...
-- ......
-- ...x..abcde
-- ...y..fghij
-- ......
-- ......
-- ......

-- -............

-- | ............
--  |............
--  |.......JIHGF
--  |.......EDCBA
--  -............
--  -......
--  |......
--  |......
--  |......
--  |......
--  -......
--  -......
--  |...x..abcde
--  |...y..fghij
--  |......
--  |......
--  -......

--       %%%%%%############
--       %%%%%%############
--       %%%%Q%############
--       %%%%%%############
--       %%%%%%###q#######a
--       %%%%%%############
--       %%%%%%......
--       %%%%%%......
--       %%%%%%......
--       %%%%%%......
--       %%%%%%......
--       %%%%r%...... A
-- ..................
-- ...............x..abcde
-- ...............y..fghij
-- ..................
-- ..................
-- ..................
--             .........FA.
--             .........GB.
--             .........HC.
--             .........ID.
--             .........JE.
--             ............
