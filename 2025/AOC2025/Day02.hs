{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2025.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.
module AOC2025.Day02 (
  day02a,
  day02b,
  isDupper
)
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntMap.NonEmpty as NEIM
import qualified Data.IntSet as IS
import qualified Data.IntSet.NonEmpty as NEIS
import qualified Data.List.NonEmpty as NE
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map as M
import qualified Data.Map.NonEmpty as NEM
import qualified Data.OrdPSQ as PSQ
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

day02a :: [(Int, Int)] :~> _
day02a =
  MkSol
    { sParse =
          traverse (listTup . map read . splitOn "-") . splitOn ","
    , sShow = show
    , sSolve =
        noFail $
          sum . filter (isDup . show) . concatMap (\(x, y) -> [x .. y])
    }
  where
    isDup xs = a == b && even (length xs)
      where
        (a, b) = splitAt (length xs `div` 2) xs

day02b :: _ :~> _
day02b =
  MkSol
    { sParse = sParse day02a
    , sShow = show
    , sSolve =
        noFail $
          sum . filter (isDupper . show) . concatMap (\(x, y) -> [x .. y])
    }

isDupper xs = not $ null $ do
  (a,b) <- zip (inits xs) (tails xs)
  guard $ not (null a)
  guard $ not (null b)
  guard $ length b `mod` length a == 0
  let match = zip (cycle a) b
  guard $ all (uncurry (==)) match
