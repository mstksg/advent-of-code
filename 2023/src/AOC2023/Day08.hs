{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
module AOC2023.Day08 (
  day08a,
  day08b,
)
where

import AOC.Common (LCM (..))
import AOC.Solver (noFail, (:~>) (..))
import Control.Monad (guard)
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

parseMe :: [String] -> Maybe ([Bool], [(String, String, String)])
parseMe = \case
  x : _ : xs ->
    let path = map (== 'R') x
        mp =
          [ (a, filter isAlphaNum b, filter isAlphaNum c)
          | [a, "=", b, c] <- words <$> xs
          ]
     in Just (path, mp)
  _ -> Nothing

stateMachine :: (String -> Bool) -> [Bool] -> [(String, String, String)] -> Map String (Seq String)
stateMachine isValid lrs xs =
  M.fromList
    [ (a, (\dir -> if dir then c else b) <$> dirMap)
    | (a, b, c) <- xs
    , isValid a
    ]
  where
    dirMap :: Seq Bool
    dirMap = Seq.fromList lrs

expandPath ::
  Map String (Seq String) ->
  Map String [String]
expandPath mp = (`Seq.index` 0) <$> res
  where
    res = flip (fmap . Seq.mapWithIndex) mp \i str ->
      str : case M.lookup str res of
        Nothing -> []
        Just r -> r `ixMod` (i + 1)

ixMod :: Seq a -> Int -> a
ixMod xs i = xs `Seq.index` (i `mod` Seq.length xs)

day08a :: ([Bool], [(String, String, String)]) :~> Int
day08a =
  MkSol
    { sParse = parseMe . lines
    , sShow = show
    , sSolve = noFail \(xs, mp) ->
        let sm = stateMachine (/= "ZZZ") xs mp
         in length $ expandPath sm M.! "AAA"
    }

day08b :: ([Bool], [(String, String, String)]) :~> Int
day08b =
  MkSol
    { sParse = parseMe . lines
    , sShow = show
    , sSolve = noFail \(xs, mp) ->
        foldr lcm 1
          . mapMaybe (\(k, i) -> guard (last k == 'A') $> length i)
          . M.toList
          . expandPath
          $ stateMachine (\k -> last k /= 'Z') xs mp
    }
