{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day08 (
  day08a,
  day08b,
)
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
import qualified Data.IntMap.NonEmpty as IM
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

-- sameLine p1@(V2 x1 y1) p2@(V2 x2 y2)
--   | dx == 0 && dy /= 0 = y1 `mod` y2 == 0 || y2 `mod` y1 == 0
--   | dy == 0 && dx /= 0 = x1 `mod` x2 == 0 || x2 `mod` x1 == 0
--   | dx /= 0 && dy /= 0 && yb `mod` ys == 0 = xb
--   where
--     d@(V2 dx dy) = pBig - pSmall
--     pBig@(V2 xb yb) = maximumBy (comparing mannNorm) [p1,p2]
--     pSmall@(V2 xm ym) = maximumBy (comparing mannNorm) [p1,p2]

isAntinode :: Map Point Char -> Point -> Bool
isAntinode mp p = any (hasDouble . sortOn mannNorm . toList) lineDists
  where
    hasDouble [] = False
    hasDouble (x : xs) = (x * 2) `elem` xs || hasDouble xs
    lineDists =
      M.fromListWith
        (<>)
        [ (c, S.singleton (p' - p))
        | (p', c) <- M.toList mp
        ]

day08a :: _ :~> _
day08a =
  MkSol
    { sParse = Just . parseAsciiMap Just
    , sShow = show
    -- , sShow = ('\n' :) . displayAsciiSet '.' '#'
    , sSolve = \mp -> do
        bb <- boundingBox' (M.keys mp)
        let noDots = M.filter (/= '.') mp
        -- pure $ S.filter (isAntinode noDots) (fillBoundingBox bb)
        pure $ countTrue (isAntinode noDots) (fillBoundingBox bb)
    }

day08b :: _ :~> _
day08b =
  MkSol
    { sParse = sParse day08a
    , sShow = show
    , sSolve = \mp -> do
        bb <- boundingBox' (M.keys mp)
        let noDots = M.filter (/= '.') mp
        -- pure $ S.filter (isAntinode noDots) (fillBoundingBox bb)
        pure . S.size . S.fromList $ do
          (p1, c1) <- M.toList noDots
          (p2, c2) <- M.toList noDots
          guard $ p1 /= p2
          guard $ c1 == c2
          let delta = p2 - p1
          takeWhile (inBoundingBox bb) $ iterate (+ delta) p2
    }
