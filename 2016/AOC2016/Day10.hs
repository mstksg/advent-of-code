-- |
-- Module      : AOC2016.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
module AOC2016.Day10 (
  day10a,
  day10b,
) where

import AOC.Solver ((:~>) (..))
import Control.DeepSeq (NFData)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Semigroup (First (..))
import GHC.Generics (Generic)

type Bot = Int

data Source
  = SInp Int
  | SBot Bool Bot
  deriving stock (Eq, Ord, Show)

data DestMap a b = DestMap
  { dmBots :: !(Map Bot a)
  , dmOuts :: !(Map Int b)
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

instance (Semigroup a, Semigroup b) => Semigroup (DestMap a b) where
  DestMap x1 y1 <> DestMap x2 y2 =
    DestMap
      (M.unionWith (<>) x1 x2)
      (M.unionWith (<>) y1 y2)

instance (Semigroup a, Semigroup b) => Monoid (DestMap a b) where
  mempty = DestMap M.empty M.empty

parseLine :: String -> DestMap [Source] (First Source)
parseLine str = case words str of
  "bot" : n : _ : _ : _ : oa : a : _ : _ : _ : ob : b : _ ->
    mkDest oa (read a) (SBot False (read n))
      <> mkDest ob (read b) (SBot True (read n))
  "value" : i : _ : _ : oa : a : _ ->
    mkDest oa (read a) (SInp (read i))
  _ -> mempty
  where
    mkDest = \case
      "bot" -> \b x -> DestMap (M.singleton b [x]) mempty
      "output" -> \i x -> DestMap mempty (M.singleton i (First x))
      _ -> error "huh"

runBots :: DestMap [Source] (First Source) -> DestMap (Int, Int) Int
runBots DestMap{..} = DestMap bmp omp
  where
    bmp =
      dmBots <&> \case
        [x, y] ->
          let i = runSource x
              j = runSource y
           in (min i j, max i j)
        _ -> error "what"

    omp = dmOuts <&> \case First s -> runSource s
    runSource = \case
      SInp i -> i
      SBot r b ->
        let (i, j) = bmp M.! b
         in if r
              then j
              else i

day10a :: DestMap [Source] (First Source) :~> Int
day10a =
  MkSol
    { sParse = Just . foldMap parseLine . lines
    , sShow = show
    , sSolve = \dm ->
        listToMaybe
          [ b
          | (b, (17, 61)) <- M.toList $ dmBots (runBots dm)
          ]
    }

day10b :: DestMap [Source] (First Source) :~> Int
day10b =
  MkSol
    { sParse = Just . foldMap parseLine . lines
    , sShow = show
    , sSolve = \dm ->
        Just
          let outs = dmOuts $ runBots dm
           in (outs M.! 0) * (outs M.! 1) * (outs M.! 2)
    }
