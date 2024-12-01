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

import AOC.Common.Parser (
  CharParser,
  between',
  fullLine,
  pAlphaWord,
  pTok,
  pWord,
  parseMaybe',
  tokenAssoc,
 )
import AOC.Solver (noFail, (:~>) (..))
import Control.Monad (guard)
import qualified Control.Monad.Combinators as P
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Text.Megaparsec.Char as P

puzzleParse :: CharParser ([Bool], [(String, String, String)])
puzzleParse = do
  ts <- fullLine . P.many $ tokenAssoc [('L', False), ('R', True)]
  P.newline
  rules <- P.many . fullLine $ do
    x <- pWord
    pTok "="
    (y, z) <- between' "(" ")" $ (,) <$> pAlphaWord <* "," <*> pAlphaWord
    pure (x, y, z)
  pure (ts, rules)

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
    { sParse = parseMaybe' puzzleParse
    , sShow = show
    , sSolve = noFail \(xs, mp) ->
        let sm = stateMachine (/= "ZZZ") xs mp
         in length $ expandPath sm M.! "AAA"
    }

day08b :: ([Bool], [(String, String, String)]) :~> Int
day08b =
  MkSol
    { sParse = parseMaybe' puzzleParse
    , sShow = show
    , sSolve = noFail \(xs, mp) ->
        foldr lcm 1
          . mapMaybe (\(k, i) -> guard (last k == 'A') $> length i)
          . M.toList
          . expandPath
          $ stateMachine (\k -> last k /= 'Z') xs mp
    }
