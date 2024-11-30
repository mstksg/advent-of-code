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

import AOC.Common.Parser (CharParser, pDecimal, pTok, pTokMany, parseLines, sepBy')
import AOC.Solver ((:~>) (..))
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Text.Megaparsec.Char as P

lineParser :: CharParser (Int, Map String Int)
lineParser = do
  "Game"
  n <- pDecimal
  ":"
  maps <-
    flip sepBy' ";" $
      M.fromList <$> do
        flip sepBy' (pTok ",") do
          i <- pDecimal
          color <- pTokMany P.letterChar
          pure (color, i)
  pure (n, M.unionsWith max maps)

day02a :: [(Int, Map String Int)] :~> Int
day02a =
  MkSol
    { sParse = parseLines lineParser
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
    { sParse = fmap (map snd) . parseLines lineParser
    , sShow = show
    , sSolve = Just . sum . map product
    }
