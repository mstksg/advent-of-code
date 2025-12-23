-- |
-- Module      : AOC2025.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
module AOC2025.Day08 (
  day08a,
  day08b,
)
where

import AOC.Common (listV3, readAll)
import AOC.Solver (dyno_, (:~>) (..))
import Control.Monad (replicateM, (<=<))
import Control.Monad.Trans.State (StateT (..), evalStateT, execStateT, gets)
import Data.Foldable (find)
import Data.List (sortOn, tails)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Linear as L
import Linear.V3 (V3 (..))

day08a :: [V3 Int] :~> _
day08a =
  MkSol
    { sParse =
        traverse (listV3 <=< readAll . splitOn ",") . lines
    , sShow = show
    , sSolve = \pts -> do
        let go = replicateM (dyno_ "pairs" 1000) chomp
        (chunks, _) <- execStateT go (S.fromList $ S.singleton <$> pts, sortedPairs pts)
        pure . product . take 3 . sortOn Down . map S.size . S.toList $ chunks
    }

day08b :: [V3 Int] :~> Int
day08b =
  MkSol
    { sParse = sParse day08a
    , sShow = show
    , sSolve = \pts -> do
        let go = do
              out <- chomp
              isOne <- gets $ (== 1) . S.size . fst
              if isOne
                then pure out
                else go
        (V3 px _ _, V3 qx _ _) <- evalStateT go (S.fromList $ S.singleton <$> pts, sortedPairs pts)
        pure $ px * qx
    }

sortedPairs :: (Ord b, L.Metric f, Num b) => [f b] -> [(f b, f b)]
sortedPairs pts = sortOn (uncurry L.qd) [(p, q) | p : ps <- tails pts, q <- ps]

chomp :: Ord b => StateT (Set (Set b), [(b, b)]) Maybe (b, b)
chomp = StateT \case
  (clusts, (p, q) : pts) -> do
    pclust <- find (p `S.member`) clusts
    clusts' <-
      if q `S.member` pclust
        then pure clusts
        else do
          qclust <- find (q `S.member`) clusts
          pure . S.insert (pclust <> qclust) . S.delete pclust . S.delete qclust $ clusts
    pure ((p, q), (clusts', pts))
  _ -> Nothing

-- 405, 1548 ... spent 15 minutes debugging inits vs tails lol
