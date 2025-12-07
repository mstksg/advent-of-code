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

import AOC.Common (countTrue)
import AOC.Common.Point (Point, parseAsciiMap)
import AOC.Solver (noFail, (:~>) (..))
import Control.Lens (view)
import Control.Monad (guard)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Semigroup (Any (..), Sum (..))
import qualified Data.Set as S
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

buildTriangle :: Point -> [Point]
buildTriangle = go 0 0
  where
    go !r !c !p =
      p
        : if r == c
          then go (r + 1) 0 (p + V2 (negate (r * 2 + 1)) 2)
          else go r (c + 1) (p + V2 2 0)

solve ::
  (Monoid m) =>
  -- | start pos
  Point ->
  -- | splitters
  NESet Point ->
  -- | value at start pos
  m ->
  -- | value out of bounds
  m ->
  -- | direction a point flows
  (Point -> [Point]) ->
  M.Map Point m
solve startPos splitters startVal boundary flow = result `M.restrictKeys` NES.toSet splitters
  where
    maxY = maximum . map (view _y) $ toList splitters
    points = S.fromList . takeWhile ((<= maxY) . view _y) $ buildTriangle (startPos + V2 0 2)
    result = flip M.fromSet points \p ->
      (if p == startPos + V2 0 2 then mappend startVal else id) $
        foldMap (\n -> M.findWithDefault boundary n result) (flow p)

day07a :: _ :~> _
day07a =
  MkSol
    { sParse = parseMap
    , sShow = show
    , sSolve =
        noFail $
          \(startPos, splitters) ->
            countTrue getAny $ solve startPos splitters (Any True) (Any False) \p ->
              [n | n <- [p - V2 1 2, p - V2 (-1) 2], n `NES.member` splitters]
                <> [n | n <- [p - V2 0 4], n `NES.notMember` splitters]
    }

day07b :: (Point, NESet Point) :~> Int
day07b =
  MkSol
    { sParse = parseMap
    , sShow = show
    , sSolve =
        \(startPos, splitters) ->
          fmap getSum . M.lookup (startPos + V2 0 2) $
            solve startPos splitters 0 1 $ \p ->
              if p `NES.member` splitters
                then [p + V2 1 2, p + V2 (-1) 2]
                else [p + V2 0 4]
    }

-- 502, 1491
