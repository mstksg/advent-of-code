{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2025.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
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
module AOC2025.Day11 (
  day11a,
  day11b,
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
import Data.Bitraversable

day11a :: [(String, [String])] :~> _
day11a =
  MkSol
    { sParse =
          traverse (bitraverse initMay pure <=< uncons . words) . lines
    , sShow = show
    , sSolve =
        noFail \xs ->
          pathsTo' (M.fromList xs) "you" "out"
    }

flipMap' :: Map String (Set String) -> Map String (Set String)
flipMap' mp = M.fromListWith (<>)
  [ (x, S.singleton k)
    | (k, xs) <- M.toList mp
  , x <- S.toList xs
  ]

day11b :: _ :~> _
day11b =
  MkSol
    { sParse = sParse day11a
    , sShow = show
    , sSolve =
        noFail \xs ->
          let toVisit = S.fromList ["dac","fft"]
           in pathsTo' (M.fromList xs `addVisited` toVisit) ("svr", toVisit) ("out", S.empty)
    }

pathsTo :: Map String (Set String) -> Set String -> String -> String -> [[String]]
pathsTo conns takeout start end = res M.! start
  where
    conns' = (`S.difference` takeout) <$> (conns `M.withoutKeys` takeout)
    res = flip M.mapWithKey conns \x nx -> flip foldMap nx \y ->
        if y == end
           then [[x, end]]
           else maybe [] (map (x:)) $ M.lookup y res

pathsToAll :: Map String (Set String) -> Map String (Map String Int)
pathsToAll conns = flip M.fromSet (fold conns) \end ->
  let res = flip M.mapWithKey conns \start nexts -> 
        sum 
          [ if y == end then 1 else M.findWithDefault 0 y res
            | y <- S.toList nexts
          ]
    in res

pathsTo' :: (Ord a, Foldable t) => Map a (t a) -> a -> a -> Int
pathsTo' conns start end = res M.! start
  where
    res = flip M.mapWithKey conns \x nx -> getSum $ flip foldMap nx \y ->
        if y == end
           then 1
           else Sum $ M.findWithDefault 0 y res

addVisited :: Ord a => Map a [a] -> Set a -> Map (a, Set a) [(a, Set a)]
addVisited conns required = M.fromList
    [ ((x, subset), map (, S.delete x subset) xs)
      | (x, xs) <- M.toList conns
    , subset <- S.fromDistinctAscList <$> filterM (\_ -> [False, True]) (S.toAscList required)
    ]

chomperoni :: Map String (Set String) -> Set String -> String -> String -> Int
chomperoni conns visitMe start end = go visitMe start
  where
    go :: Set String -> String -> Int
    go toSee curr
      | curr == end = if S.null toSee then 1 else 0
      | otherwise = sum do
          next <- S.toList $ M.findWithDefault S.empty curr conns
          pure $ go (S.delete curr toSee) next

data SearchState = S { leftOver :: !(Set String), current :: !String }
  deriving stock Show

searchBfs :: Map String (Set String) -> Set String -> String -> String -> Int
searchBfs conns visitMe start end = go (Seq.singleton (S visitMe start)) 0
  where
    go Seq.Empty tot = tot
    go (S toSee curr :<| xs) !tot
      | curr == end = go xs $ if S.null toSee then tot + 1 else tot
      | otherwise =
          let nexts = S (S.delete curr toSee) <$> foldMap toList (M.lookup curr conns)
           in go (xs <> Seq.fromList nexts) tot

-- data SearchState2 = S { leftOver2 :: !(Set String), pathsToHere :: !Int }
--   deriving stock Show

-- searchBfs' :: Map String (Set String) -> Set String -> String -> String -> Int
-- searchBfs' conns visitMe start end = go (M.singleton start (S2 visitMe pathsToHere)) 0
--   where
--     go queue tot = case M.minViewWithKey $ traceShowId queue of
--       Nothing -> tot
--       Just ((curr, toSee), xs)
--         | curr == end -> go xs $ if S.null toSee then tot + 1 else tot
--         | otherwise ->
--           let nexts = M.fromSet (\_ -> S.delete curr toSee) $ M.findWithDefault S.empty curr conns
--            in go (M.unionWith (\x y -> if S.size x < S.size y then x else y) nexts xs) tot
            
    -- go Seq.Empty tot = tot
    -- go (S toSee curr :<| xs) !tot
    --   | curr == end = go xs $ traceShowId $ if S.null toSee then tot + 1 else tot
    --   | otherwise =
    --       let nexts = traceShowId $ S (S.delete curr toSee) <$> foldMap toList (M.lookup curr conns)
    --        in go (xs <> Seq.fromList nexts) tot
