-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
module AOC2023.Day02 (
  day02a,
  day02b,
)
where

import AOC.Common (listTup)
import AOC.Solver ((:~>) (..))
import Control.Monad (guard, (<=<))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

parseLine :: String -> Maybe (Int, Map String Int)
parseLine fullLine = do
  (gameNum, specs) <- listTup $ splitOn ": " fullLine
  ("Game", n) <- listTup $ words gameNum
  i <- readMaybe n
  sets <-
    fmap (M.unionsWith max)
      . traverse
        ( fmap M.fromList
            . traverse (traverse readMaybe <=< listTup . reverse . words)
            . splitOn ","
        )
      . splitOn ";"
      $ specs
  pure (i, sets)

day02a :: [(Int, Map String Int)] :~> Int
day02a =
  MkSol
    { sParse = traverse parseLine . lines
    , sShow = show
    , sSolve = Just . sum . mapMaybe (\(a, b) -> a <$ guard (isLegal b))
    }
  where
    maxMap =
      M.fromList
        [ ("red", 12)
        , ("green", 13)
        , ("blue", 14)
        ]
    isLegal = and . M.intersectionWith (>=) maxMap

day02b :: [Map String Int] :~> Int
day02b =
  MkSol
    { sParse = fmap (map snd) . traverse parseLine . lines
    , sShow = show
    , sSolve = Just . sum . map product
    }
