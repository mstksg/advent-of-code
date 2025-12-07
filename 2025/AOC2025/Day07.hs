-- |
-- Module      : AOC2025.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
module AOC2025.Day07 (
  day07a,
  day07b,
)
where

import AOC.Common (countTrue, firstJust)
import AOC.Common.Point (Point, parseAsciiMap)
import AOC.Solver (noFail, (:~>) (..))
import Control.Lens (view)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEM
import Data.Maybe (fromMaybe)
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import Linear.V2 (V2 (..), _y)

parseMap :: String -> Maybe (Point, NESet Point)
parseMap =
  reshape
    . M.partition id
    . parseAsciiMap \case 'S' -> Just True; '^' -> Just False; _ -> Nothing
  where
    reshape (startPos, rest) = (,) . fst <$> M.lookupMin startPos <*> NES.nonEmptySet (M.keysSet rest)

day07a :: (Point, NESet Point) :~> Int
day07a =
  MkSol
    { sParse = parseMap
    , sShow = show
    , sSolve =
        noFail $ \(startPos, splitters) ->
          let pathsTo :: NEMap Point Bool
              pathsTo = flip NEM.fromSet splitters \(V2 x y0) ->
                let cands = takeWhile ((`NES.notMember` splitters) . V2 x) [y0 - 2, y0 - 4 .. 0]
                 in flip any cands \y ->
                      V2 x y == startPos
                        || NEM.findWithDefault False (V2 (x - 1) y) pathsTo
                        || NEM.findWithDefault False (V2 (x + 1) y) pathsTo
           in countTrue id pathsTo
    }

day07b :: (Point, NESet Point) :~> Int
day07b =
  MkSol
    { sParse = parseMap
    , sShow = show
    , sSolve = \(startPos, splitters) -> do
        let maxY = maximum . map (view _y) $ toList splitters
            downFrom (V2 x y0) = fromMaybe 1 $ flip firstJust [y0, y0 + 2 .. maxY] \y ->
              NEM.lookup (V2 x y) pathsFrom
            pathsFrom :: NEMap Point Int
            pathsFrom = flip NEM.fromSet splitters \p ->
              downFrom (p + V2 1 2) + downFrom (p + V2 (-1) 2)
        NEM.lookup (startPos + V2 0 2) pathsFrom
    }

-- 502, 1491
