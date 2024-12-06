{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day06 (
  day06a,
  day06b,
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

day06a :: _ :~> _
day06a =
  MkSol
    { sParse =
        noFail $
          parseAsciiMap \case '#' -> Just True; '^' -> Just False; _ -> Nothing
    , sShow = show
    , sSolve =
        noFail \mp ->
          let boulds = M.keysSet $ M.filter id mp
              startPos = S.findMin . M.keysSet $ M.filter not mp
              Just bb = boundingBox' $ M.keysSet mp
              path = flip iterateMaybe (startPos, South) \(x, d) ->
                let newpos = x + dirPoint d
                    good = inBoundingBox bb newpos
                 in guard good
                      $> if newpos `S.member` boulds
                        then (x, d <> West)
                        else (newpos, d)
           in S.size . S.fromList $ fst <$> path
    }

day06b :: _ :~> _
day06b =
  MkSol
    { sParse = sParse day06a
    , sShow = show
    , sSolve =
        noFail \mp ->
          let boulds = M.keysSet $ M.filter id mp
              startPos = S.findMin . M.keysSet $ M.filter not mp
              Just bb = boundingBox' $ M.keysSet mp
              findLoop boulds' = findLoopBy id $ flip iterateMaybe (startPos, South) $ \(x, d) ->
                let newpos = x + dirPoint d
                    good = inBoundingBox bb newpos
                 in guard good
                      $> if newpos `S.member` boulds'
                        then (x, d <> West)
                        else (newpos, d)
           in flip countTrue (fillBoundingBox bb) \p -> traceShow p $
                not (p `M.member` mp) && isJust (findLoop (S.insert p boulds))
    }
