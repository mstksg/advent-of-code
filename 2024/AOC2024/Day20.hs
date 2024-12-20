{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day20 (
  day20a,
  day20b,
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

day20a :: _ :~> _
day20a =
  MkSol
    { sParse =
        noFail $ parseAsciiMap \case
          'S' -> Just $ Just False
          'E' -> Just $ Just True
          '#' -> Just Nothing
          _ -> Nothing
    , sShow = show
    , sSolve = \mp -> do
        start : _ <- pure . M.keys $ M.filter (== Just False) mp
        end : _ <- pure . M.keys $ M.filter (== Just True) mp
        bb <- boundingBox' $ M.keysSet mp
        let walls = M.keysSet $ M.filter isNothing mp
        goodPath <-
          let go p = cardinalNeighbsSet p `S.difference` walls
           in length <$> bfs go start (== end)
        traceM $ show goodPath
        let reachable = floodFill (\p -> cardinalNeighbsSet p `S.difference` walls) (S.singleton start)
            cheats =
              S.fromList
                [ (w, d)
                | w <- toList walls
                , dir <- [ North ..]
                , let d = w + dirPoint dir
                      d' = w - dirPoint dir
                , d `S.notMember` walls
                , d' `S.member` reachable
                -- toList $ cardinalNeighbsSet w `S.difference` walls
                , inBoundingBox bb d
                , inBoundingBox bb d'
                ]
            cheatPaths = do
              (w, d) <- toList cheats
              let go p = cardinalNeighbsSet p `S.difference` S.delete w walls
                  go' p = cardinalNeighbsSet p `S.difference` walls
              traceM $ show (w, d)
              getToCheat <- maybeToList $ length <$> bfs go start (== w)
              -- getToCheat <- maybeToList $ fst <$> aStar (mannDist w) (M.fromSet (const 1) . go) start (== w)
              -- guard $ getToCheat + mannDist d end < goodPath - 100
              -- traceM $ show getToCheat
              getToEnd <- maybeToList $ (+ 1) . length <$> bfs go' d (== end)
              -- getToEnd <- maybeToList $ (+1) . fst <$> aStar (mannDist end) (M.fromSet (const 1) . go') d (== end)
              -- traceM $ show (getToEnd, getToCheat + getToEnd)
              pure $! getToCheat + getToEnd
        pure $ countTrue (\t -> traceShowId (goodPath - t) >= 100) cheatPaths
    }

-- aStar ::
--   forall n p.
--   (Ord n, Ord p, Num p) =>
--   -- | heuristic
--   (n -> p) ->
--   -- | neighborhood
--   (n -> Map n p) ->
--   -- | start
--   n ->
--   -- | target
--   (n -> Bool) ->
--   -- | the shortest path, if it exists, and its cost
--   Maybe (p, [n])

data CheatState = PreCheat
                | InCheat Int
                | PostCheat
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NFData

-- okay new plan:
-- * use spTree from end and spTree from beginning (fgl)
-- * pair up each item in each, that is within range of a "teleport" and would
-- give a good enough time
day20b :: _ :~> _
day20b =
  MkSol
    { sParse = sParse day20a
    -- , sShow = unlines . map show
    , sShow = show . length
    , sSolve = \mp -> do
        start : _ <- pure . M.keys $ M.filter (== Just False) mp
        end : _ <- pure . M.keys $ M.filter (== Just True) mp
        bb <- boundingBox' $ M.keysSet mp
        let walls = M.keysSet $ M.filter isNothing mp
        goodPath <-
          let go p = cardinalNeighbsSet p `S.difference` walls
           in length <$> bfs go start (== end)
        let goCheat (p, st) = S.filter (inBoundingBox bb . fst) $ case st of
              PreCheat -> S.map (, PreCheat) (cardinalNeighbsSet p `S.difference` walls)
                       <> S.map (, InCheat 20) (cardinalNeighbsSet p `S.intersection` walls)
              InCheat i
                | i > 2 -> S.map (, InCheat (i - 1)) (cardinalNeighbsSet p)
                | otherwise -> S.map (, PostCheat) (cardinalNeighbsSet p `S.difference` walls)
              PostCheat -> S.map (,PostCheat) (cardinalNeighbsSet p `S.difference` walls)
            cheatIx ps = do
              here <- listToMaybe
                [p
                  | (p, InCheat _) <- ps ]
              there <- listToMaybe
                [p
                  | (p, PostCheat) <- ps ]
              pure (V2 here there, length ps)
              -- (length xs, length zs)
              --   (xs, ys) = span ((== PreCheat) . snd) ps
              --   zs = dropWhile ((/= PostCheat) . snd) ys
            allPaths = mapMaybe (traceShowId . cheatIx) $ go 0 S.empty (start, PreCheat)
              where
                go n seen s = do
                  guard $ n < goodPath - 50
                  s'@(p, _) <- toList $ goCheat s
                  guard $ p `S.notMember` seen
                  (s':) <$> if s' == (end, PostCheat)
                     then pure []
                     else go (n + 1) (S.insert p seen) s'
        -- pure $
        --   let go p = cardinalNeighbsSet p `S.difference` walls
        --    in bfsAll go start (== end)
        pure . M.toList . M.filter (>= 50) . fmap (goodPath -) $ M.fromListWith min allPaths
          -- let go (p, st) = S.filter (inBoundingBox bb . fst) $ case st of
          --       PreCheat -> S.map (, PreCheat) (cardinalNeighbsSet p `S.difference` walls)
          --                <> S.map (, InCheat 2) (cardinalNeighbsSet p `S.intersection` walls)
          --       InCheat i
          --         | i > 2 -> S.map (, InCheat (i - 1)) (cardinalNeighbsSet p)
          --         | otherwise -> S.map (, PostCheat) (cardinalNeighbsSet p `S.difference` walls)
          --       PostCheat -> S.map (,PostCheat) (cardinalNeighbsSet p `S.difference` walls)
          --  in bfsAll go (start, PreCheat) ((== end) . fst)
        -- traceM $ show goodPath
        -- let reachable = floodFill (\p -> cardinalNeighbsSet p `S.difference` walls) (S.singleton start)
        --     cheats =
        --       S.fromList
        --         [ (w, d)
        --         | w <- toList walls
        --         , dir <- [ North ..]
        --         , let d = w + dirPoint dir
        --               d' = w - dirPoint dir
        --         , d `S.notMember` walls
        --         , d' `S.member` reachable
        --         -- toList $ cardinalNeighbsSet w `S.difference` walls
        --         , inBoundingBox bb d
        --         , inBoundingBox bb d'
        --         ]
        --     cheatPaths = do
        --       (w, d) <- toList cheats
        --       let go p = cardinalNeighbsSet p `S.difference` S.delete w walls
        --           go' p = cardinalNeighbsSet p `S.difference` walls
        --       traceM $ show (w, d)
        --       getToCheat <- maybeToList $ length <$> bfs go start (== w)
        --       -- getToCheat <- maybeToList $ fst <$> aStar (mannDist w) (M.fromSet (const 1) . go) start (== w)
        --       -- guard $ getToCheat + mannDist d end < goodPath - 100
        --       -- traceM $ show getToCheat
        --       getToEnd <- maybeToList $ (+ 1) . length <$> bfs go' d (== end)
        --       -- getToEnd <- maybeToList $ (+1) . fst <$> aStar (mannDist end) (M.fromSet (const 1) . go') d (== end)
        --       -- traceM $ show (getToEnd, getToCheat + getToEnd)
        --       pure $! getToCheat + getToEnd
        -- pure $ countTrue (\t -> traceShowId (goodPath - t) >= 100) cheatPaths
    }

-- data BFSState n = BS
--   { _bsClosed :: !(Map n (Maybe n))
--   -- ^ map of item to "parent"
--   , _bsOpen :: !(Seq n)
--   -- ^ queue
--   }

-- -- | Breadth-first search, with loop detection
-- bfsAll ::
--   forall n.
--   Ord n =>
--   -- | neighborhood
--   (n -> Set n) ->
--   -- | start
--   n ->
--   -- | target
--   (n -> Bool) ->
--   -- | the shortest path, if it exists
--   [[n]]
-- bfsAll ex x0 dest = reconstruct <$> go (addBack x0 Nothing (BS M.empty Seq.empty))
--   where
--     reconstruct :: (n, Map n (Maybe n)) -> [n]
--     reconstruct (goal, mp) = drop 1 . reverse $ goreco goal
--       where
--         goreco n = n : maybe [] goreco (mp M.! n)
--     go :: BFSState n -> [(n, Map n (Maybe n))]
--     go BS{..} = case _bsOpen of
--       Empty -> []
--       n :<| ns
--         | dest n -> (n, _bsClosed) : go (BS _bsClosed ns)
--         | otherwise -> go . S.foldl' (processNeighbor n) (BS _bsClosed ns) $ ex n
--     addBack :: n -> Maybe n -> BFSState n -> BFSState n
--     addBack x up BS{..} =
--       BS
--         { _bsClosed = M.insert x up _bsClosed
--         , _bsOpen = _bsOpen :|> x
--         }
--     processNeighbor :: n -> BFSState n -> n -> BFSState n
--     processNeighbor curr bs0@BS{..} neighb
--       | neighb `M.member` _bsClosed = bs0
--       | otherwise = addBack neighb (Just curr) bs0

