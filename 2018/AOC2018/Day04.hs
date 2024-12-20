-- |
-- Module      : AOC2018.Day04
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
module AOC2018.Day04 (
  day04a,
  day04b,
) where

import AOC.Common (clearOut, freqs, maximumVal, maximumValBy)
import AOC.Solver ((:~>) (..))
import AOC.Util (eitherToMaybe)
import Control.Applicative (many)
import Data.Char (isAlphaNum)
import Data.Finite (Finite, packFinite)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import qualified Text.Parsec as P
import Text.Read (readMaybe)

type Minute = Finite 60

-- | Map of minutes to times slept at that minute
type TimeCard = Map Minute Int

-- | Rudimentary time tuple
data Time = T
  { _tYear :: Integer
  , _tMonth :: Integer
  , _tDay :: Integer
  , _tHour :: Finite 24
  , _tMinute :: Minute
  }
  deriving stock (Show, Eq, Ord)

-- | A guard ID.  It's a newtype to prevent us from accidentally mixing up
-- all of the integer types involved.
newtype Guard = G {_gId :: Int}
  deriving stock (Show, Eq, Ord)

-- | A logged action
data Action
  = AShift Guard
  | ASleep
  | AWake
  deriving stock (Show, Eq, Ord)

-- | Parse a stream of @('Time', 'Action')@ events
type Parser = P.Parsec [(Time, Action)] ()

-- | From a stream of @('Time', 'Action')@ events, accumulate a map of
-- guards to time cards.
buildTimeCards :: Map Time Action -> Maybe (Map Guard TimeCard)
buildTimeCards = eitherToMaybe . P.parse fullLog "" . M.toList
  where
    -- \| A log is many guard shifts.  The result is a frequency map of all
    -- of the guards' accumulated minutes.
    fullLog :: Parser (Map Guard TimeCard)
    fullLog = fmap freqs . M.fromListWith (++) <$> many guardShift
    -- \| A shift is a guard chagen with many naps.  Returns the minues
    -- slept, with the guard ID.
    guardShift :: Parser (Guard, [Minute])
    guardShift = do
      (_, AShift g) <- P.anyToken
      napMinutes <- concat <$> many (P.try nap)
      pure (g, napMinutes)
    -- \| A nap is a sleep then a wake.  Return the minutes slept.
    nap :: Parser [Minute]
    nap = do
      (T _ _ _ _ m0, ASleep) <- P.anyToken
      (T _ _ _ _ m1, AWake) <- P.anyToken
      pure [m0 .. m1 - 1]

day04a :: Map Time Action :~> Int
day04a =
  MkSol
    { sParse = fmap M.fromList . traverse parseLine . lines
    , sShow = show
    , sSolve = \logs -> do
        timeCards <- buildTimeCards logs
        (worstGuard, timeCard) <- maximumValBy (comparing sum) timeCards
        (worstMinute, _) <- maximumVal timeCard
        pure $ _gId worstGuard * fromIntegral worstMinute
    }

day04b :: Map Time Action :~> Int
day04b =
  MkSol
    { sParse = fmap M.fromList . traverse parseLine . lines
    , sShow = show
    , sSolve = \logs -> do
        timeCards <- buildTimeCards logs
        let worstMinutes :: Map Guard (Minute, Int)
            worstMinutes = M.mapMaybe maximumVal timeCards
        (worstGuard, (worstMinute, _)) <- maximumValBy (comparing snd) worstMinutes
        pure $ _gId worstGuard * fromIntegral worstMinute
    }

parseLine :: String -> Maybe (Time, Action)
parseLine str = do
  [y, mo, d, h, mi] <- traverse readMaybe timeStamp
  t <- T y mo d <$> packFinite h <*> packFinite mi
  a <- case rest of
    "falls" : "asleep" : _ -> Just ASleep
    "wakes" : "up" : _ -> Just AWake
    "Guard" : n : _ -> AShift . G <$> readMaybe n
    _ -> Nothing
  pure (t, a)
  where
    (timeStamp, rest) =
      splitAt 5
        . words
        . clearOut (not . isAlphaNum)
        $ str
