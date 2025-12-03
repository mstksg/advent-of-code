-- |
-- Module      : AOC2016.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
module AOC2016.Day01 (
  day01a,
  day01b,
) where

import AOC.Common (firstRepeated)
import AOC.Common.Point (Dir (..), Point, dirPoint, mannDist, parseDir)
import AOC.Solver ((:~>) (..))
import Text.Read (readMaybe)

data Turtle = (:@)
  { tLoc :: Point
  , tDir :: Dir
  }
  deriving stock (Show)

data Command
  = CTurn Dir
  | CGo
  deriving stock (Show)

stepper :: Turtle -> Command -> Turtle
stepper (x :@ h) = \case
  CTurn d -> (x + dirPoint h') :@ h'
    where
      h' = d <> h
  CGo -> (x + dirPoint h) :@ h

day01a :: [Command] :~> Int
day01a =
  MkSol
    { sParse = parseCmd
    , sShow = show
    , sSolve =
        Just
          . mannDist 0
          . tLoc
          . foldl' stepper (0 :@ North)
    }

day01b :: [Command] :~> Int
day01b =
  MkSol
    { sParse = parseCmd
    , sShow = show
    , sSolve =
        fmap (mannDist 0)
          . firstRepeated
          . map tLoc
          . scanl stepper (0 :@ North)
    }

parseCmd :: String -> Maybe [Command]
parseCmd str = Just $ do
  x : xs <- words . filter (/= ',') $ str
  Just h <- pure $ parseDir x
  Just n <- pure $ readMaybe xs
  CTurn h : replicate (n - 1) CGo
