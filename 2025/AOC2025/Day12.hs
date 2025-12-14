{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2025.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
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
module AOC2025.Day12 (
day12a,
day12b
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

pVec :: String -> Point
pVec xs = case read <$> splitOn "x" (init xs) of
                  [x,y] -> V2 x y
              

day12a :: _ :~> _
day12a =
  MkSol
    { sParse =
        noFail $ \xs ->
          let chunkies = splitOn [""] $ lines xs
              blockies = map (parseAsciiSet (== '#') . unlines . tail) $ init chunkies
              thingies = map (bimap pVec (map (read @Int)). fromJust . uncons . words) $ last chunkies
           in (blockies, thingies)
    , sShow = show
    , sSolve =
        noFail \(blocks, areas) -> countTrue (\(bounds, ns) -> (product bounds) >= (sum $ zipWith (*) (map S.size blocks) ns)) areas
        -- noFail \(blocks, areas) -> map (\(bounds, ns) -> (product bounds, sum $ zipWith (*) (map S.size blocks) ns)) areas
        -- noFail \(blocks, areas) -> countTrue (go blocks) areas
    }
  where
    -- go :: [Set Point] -> (Point, [Int]) -> Bool
    -- go blocks (bounds, repeats) = not . null $ execStateT (zipWithM go2 blocks repeats) S.empty
    --   where
    --     go2 block rep = replicateM rep $ StateT (map ((),) . placer bounds block)
    go :: [Set Point] -> (Point, [Int]) -> Bool
    go blocks (bounds, repeats) = isJust $ traceShowId $ bfs
          (popper bounds)
          state0
          (M.null . sToPlace)
      where
        state0 = S (M.filter (> 0) $ M.fromList (zip blocks repeats)) S.empty

placer :: Point -> Set Point -> Set Point -> [Set Point]
placer bounds x placed = nubOrd $ do
    rot <- S.toList . S.fromList $ toList allD8 <&> \d -> map ((+ 1) . orientPoint d . subtract 1) $ S.toList x
    coord <- toList $ fillBoundingBox (V2 0 (bounds - 3))
    let place = S.fromList $ (+ coord) <$> rot
    guard . null $ placed `S.intersection` place
    pure $ placed <> place

data SearchState = S { sToPlace :: Map (Set Point) Int, sPlaced :: Set Point }
  deriving stock (Show, Eq, Ord)

popper :: Point -> SearchState -> Set SearchState
popper bounds (S toPlace placed) = traceShowId $ S.fromList do
    (x, _) <- M.toList toPlace
    rot <- S.toList . S.fromList $ toList allD8 <&> \d -> map ((+ 1) . orientPoint d . subtract 1) $ S.toList x
    coord <- toList $ fillBoundingBox (V2 0 (bounds - 3))
    let place = S.fromList $ (+ coord) <$> rot
    guard . null $ placed `S.intersection` place
    let placed' = placed <> place
        toPlace' = M.update (mfilter (> 0) . Just . subtract 1) x toPlace
    pure $ S toPlace' placed'

allPlacements :: Point -> Set Point -> Set (Set Point)
allPlacements bounds x = S.fromList do
    rot <- S.toList . S.fromList $ toList allD8 <&> \d -> map ((+ 1) . orientPoint d . subtract 1) $ S.toList x
    coord <- toList $ fillBoundingBox (V2 0 (bounds - 3))
    pure $ S.fromList $ (+ coord) <$> rot

-- placeAll

-- bfs ::
--   forall n.
--   Ord n =>
--   -- | neighborhood
--   (n -> Set n) ->
--   -- | start
--   n ->
--   -- | target
--   (n -> Bool) ->
--   -- | the shortest path, if it exists
--   Maybe [n]

day12b :: _ :~> _
day12b =
  MkSol
    { sParse = sParse day12a
    , sShow = show
    , sSolve =
        noFail $
          id
    }
