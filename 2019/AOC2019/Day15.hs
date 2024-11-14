{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : AOC2019.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!
module AOC2019.Day15 (
  day15a,
  day15b,
) where

import AOC.Common (floodFillCount)
import AOC.Common.Point (Dir (..), Point, dirPoint)
import AOC.Common.Search (bfs)
import AOC.Solver ((:~>) (..))
import AOC2019.Common.Intcode (Memory, VMErr, parseMem, stepForever, untilHalt)
import Control.Applicative (empty)
import Control.DeepSeq (NFData)
import Data.Conduino (Pipe, squeezePipe)
import Data.Functor.Identity (Identity (..))
import Data.Semigroup (Arg (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import GHC.Generics (Generic)
import Safe (lastMay)

data Tile = Floor | Oxygen
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data Spot = S
  { sCoord :: !Point
  , sTile :: !Tile
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

type Bot = Int -> Pipe Int Int Void Identity ()

-- | We use 'Arg' becase we only compare on the 'Spot', not the 'Bot'
type BotState = Arg Spot Bot

findOxygen :: Memory -> Maybe [BotState]
findOxygen mem =
  bfs
    stepAround
    (Arg (S 0 Floor) initBot)
    (\(Arg (S _ t) _) -> t == Oxygen)
  where
    initBot :: Bot
    initBot = c
      where
        Identity ([], Left c) = squeezePipe (untilHalt (stepForever @VMErr mem))

stepAround :: BotState -> Set BotState
stepAround (Arg S{..} bot) = S.fromList $ do
  dir <- [North ..]
  let p = sCoord + dirPoint dir
  (o : _, Left c) <- pure . runIdentity $ squeezePipe (bot (dNum dir))
  case o of
    1 -> pure $ Arg (S p Floor) c
    2 -> pure $ Arg (S p Oxygen) c
    _ -> empty

dNum :: Dir -> Int
dNum = \case
  North -> 1
  East -> 4
  South -> 2
  West -> 3

day15a :: Memory :~> Int
day15a =
  MkSol
    { sParse = parseMem
    , sShow = show
    , sSolve = fmap length . findOxygen
    }

day15b :: Memory :~> Int
day15b =
  MkSol
    { sParse = parseMem
    , sShow = show
    , sSolve = \m -> do
        a0 <- lastMay =<< findOxygen m
        Just . fst $ floodFillCount stepAround (S.singleton a0)
    }
