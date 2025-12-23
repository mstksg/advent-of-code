-- |
-- Module      : AOC2025.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day06 (
  day06a,
  day06b,
)
where

import AOC.Common (readAll)
import AOC.Solver (noFail, (:~>) (..))
import Control.Monad ((<=<))
import Data.Char (isSpace)
import Data.List (transpose, uncons)
import Data.List.Split (splitWhen)
import Safe (initMay, lastMay)

parseOp :: (Foldable t, Num a) => Char -> Maybe (t a -> a)
parseOp '*' = Just product
parseOp '+' = Just sum
parseOp _ = Nothing

day06a :: [[String]] :~> Int
day06a =
  MkSol
    { sParse = noFail $ map words . lines
    , sShow = show
    , sSolve =
        fmap sum . traverse (uncurry go <=< uncons . reverse) . transpose
    }
  where
    go [c] xs = parseOp c <*> readAll xs
    go _ _ = Nothing

day06b :: [String] :~> Int
day06b =
  MkSol
    { sParse = noFail lines
    , sShow = show
    , sSolve =
        fmap sum . traverse (uncurry go <=< uncons) . splitWhen (all isSpace) . transpose
    }
  where
    go :: String -> [String] -> Maybe Int
    go xAndOp xs = do
      f <- parseOp =<< lastMay xAndOp
      x <- initMay xAndOp
      f <$> readAll (x : xs)

-- 14, 148
