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
import qualified Data.Interval as IV
import qualified Data.IntervalSet as IVS
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

parseMap :: String -> Maybe (NESet Point, Point)
parseMap str = (,) <$> NES.nonEmptySet (M.keysSet boulders) <*> S.lookupMin (M.keysSet startPos)
  where
    (boulders, startPos) = M.partition id . flip parseAsciiMap str $ \case
      '#' -> Just True
      '^' -> Just False
      _ -> Nothing

-- | Could be infinite
stepPath :: V2 Point -> NESet Point -> Point -> [(Point, Dir)]
stepPath bb boulders = iterateMaybe go . (,South)
  where
    go (x, d) =
      guard (inBoundingBox bb x)
        $> if x' `NES.member` boulders
          then (x, d <> West)
          else (x', d)
      where
        x' :: Point
        x' = x + dirPoint d

day06a :: (NESet Point, Point) :~> _
day06a =
  MkSol
    { sParse = parseMap
    , sShow = show
    , sSolve =
        noFail \(boulders, startPos) ->
          subtract 1 . S.size . S.fromList $
            fst <$> stepPath (boundingBox boulders) boulders startPos
    }

-- | Could be infinite
stepPath' :: V2 (Map Int (Set Int)) -> Point -> [(Point, Dir)]
stepPath' (V2 xMap yMap) = iterateMaybe go . (,South)
  where
    go (V2 x y, d) = case d of
      North -> do
        let ys = M.findWithDefault mempty x xMap
        y' <- subtract 1 <$> S.lookupGT y ys
        pure (V2 x y', West)
      East -> do
        let xs = M.findWithDefault mempty y yMap
        x' <- subtract 1 <$> S.lookupGT x xs
        pure (V2 x' y, North)
      South -> do
        let ys = M.findWithDefault mempty x xMap
        y' <- (+ 1) <$> S.lookupLT y ys
        pure (V2 x y', East)
      West -> do
        let xs = M.findWithDefault mempty y yMap
        x' <- (+ 1) <$> S.lookupLT x xs
        pure (V2 x' y, South)

day06b :: (NESet Point, Point) :~> Int
day06b =
  MkSol
    { sParse = sParse day06a
    , sShow = show
    , sSolve =
        noFail \(boulders, startPos) ->
          let bb = boundingBox boulders
              origPath = S.fromList $ fst <$> stepPath bb boulders startPos
              V2 xMap yMap = collapseAxes boulders
           in flip countTrue origPath \(V2 x y) ->
                let xMap' = M.insertWith (<>) x (S.singleton y) xMap
                    yMap' = M.insertWith (<>) y (S.singleton x) yMap
                 in V2 x y /= startPos && findLoop_ (stepPath' (V2 xMap' yMap') startPos)
    }
