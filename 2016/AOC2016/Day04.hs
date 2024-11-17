-- |
-- Module      : AOC2016.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
module AOC2016.Day04 (
  day04a,
  day04b,
) where

import AOC.Common (caeser, firstJust, freqList)
import AOC.Solver ((:~>) (..))
import Control.Monad (guard)
import Data.Finite (modulo)
import Data.List (isInfixOf)
import Data.List.Split (splitOneOf)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Room = Room
  { rName :: [String]
  , rId :: Int
  }
  deriving stock (Show)

parseRoom :: String -> Maybe Room
parseRoom str = do
  _ : c : n : rs <- Just . reverse . splitOneOf "-[]" $ str
  guard
    . all (uncurry (==))
    . zip c
    . map snd
    . freqList
    . concat
    $ rs
  Room (reverse rs) <$> readMaybe n

day04a :: _ :~> _
day04a =
  MkSol
    { sParse = Just . mapMaybe parseRoom . lines
    , sShow = show
    , sSolve = Just . sum . map rId
    }

day04b :: _ :~> _
day04b =
  MkSol
    { sParse = Just . mapMaybe parseRoom . lines
    , sShow = show
    , sSolve = firstJust $ \(Room n i) ->
        i <$ do
          guard $ "north" `isInfixOf` (concatMap . map) (caeser (modulo (fromIntegral i))) n
    }
