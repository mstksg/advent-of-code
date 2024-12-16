{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day16 (
  day16a,
  day16b,
)
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.Internal.Heap as Heap
import qualified Data.IntMap as IM
import qualified Data.IntMap.NonEmpty as IM
import qualified Data.IntSet as IS
import qualified Data.IntSet.NonEmpty as NEIS
import qualified Data.List.NonEmpty as NE
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map as M
import qualified Data.Map.NonEmpty as NEM
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import qualified Data.OrdPSQ as Q
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

day16a :: _ :~> _
day16a =
  MkSol
    { sParse =
        noFail $
          parseAsciiMap \case
            '#' -> Just Nothing
            'S' -> Just (Just False)
            'E' -> Just (Just True)
            _ -> Nothing
    , sShow = show
    , sSolve = \mp ->
        let start : _ = M.keys $ M.filter (== Just False) mp
            end : _ = M.keys $ M.filter (== Just True) mp
            walls = M.keysSet $ M.filter (== Nothing) mp
            heur (p, _) = mannDist p end
            step (p, d) =
              M.fromList
                [ ((p, d'), 1000)
                | d' <- [North ..]
                , d' /= d
                ]
                <> if (p + dirPoint d) `S.member` walls
                  then mempty
                  else M.singleton ((p + dirPoint d), d) 1
         in fst <$> aStar heur step (start, East) ((== end) . fst)
    }

-- data AStarState n p = AS
--   { _asClosed :: !(Map n (Maybe n))
--   -- ^ map of item to "parent"
--   , _asOpen :: !(OrdPSQ n p (p, Maybe n))
--   -- ^ map of item to "parent", and cost-so-far
--   }

-- -- | A* Search
-- aStarAll ::
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
--   [(p, [n])]
-- aStarAll h ex x0 dest = second reconstruct <$> go (addBack x0 0 Nothing (AS M.empty Q.empty))
--   where
--     reconstruct :: (n, Map n (Maybe n)) -> [n]
--     reconstruct (goal, mp) = reverse $ goreco goal
--       where
--         goreco n = n : maybe [] goreco (mp M.! n)
--     go :: AStarState n p -> [(p, (n, Map n (Maybe n)))]
--     -- go :: AStarState n p -> Maybe (p, (n, Map n (Maybe n)))
--     go as0@AS{..} =
--       maybeToList (Q.minView _asOpen) >>= \(n, p, (g, up), queue') ->
--         let closed' = M.insert n up _asClosed
--             found = [(p, (n, closed')) | dest n]
--          in -- then Just (p, (n, closed'))
--             found
--               ++ (go . M.foldlWithKey' (processNeighbor n g) (as0{_asOpen = queue', _asClosed = closed'}) $ ex n)
--     addBack :: n -> p -> Maybe n -> AStarState n p -> AStarState n p
--     addBack x g up as0 = as0{_asOpen = insertIfBetter x (g + h x) (g, up) . _asOpen $ as0}
--     processNeighbor :: n -> p -> AStarState n p -> n -> p -> AStarState n p
--     processNeighbor curr currCost as0@AS{..} neighb moveCost
--       --     | neighb `Q.member` _asOpen || neighb `M.member` _asClosed = as0
--       | neighb `M.member` _asClosed = as0
--       | otherwise = addBack neighb (currCost + moveCost) (Just curr) as0

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

-- data BFSState n = BS
--   { _bsClosed :: !(Map n (Maybe n))
--   -- ^ map of item to "parent"
--   , _bsOpen :: !(Seq n)
--   -- ^ queue
--   , _bsFound :: ![[n]]
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
--   -- | paths
--   [[n]]
-- bfsAll ex x0 dest = reconstruct <$> go (addBack x0 Nothing (BS M.empty Seq.empty []))
--   where
--     reconstruct :: (n, Map n (Maybe n)) -> [n]
--     reconstruct (goal, mp) = reverse $ goreco goal
--       where
--         goreco n = n : maybe [] goreco (mp M.! n)
--     go :: BFSState n -> [(n, Map n (Maybe n))]
--     go BS{..} = case _bsOpen of
--       Empty -> []
--       n :<| ns
--         | dest n -> [(n, _bsClosed)]
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

day16b :: _ :~> _
day16b =
  MkSol
    { sParse = sParse day16a
    , sShow = show
    , sSolve = \mp -> do
        (start, _) <- M.lookupMin $ M.filter (== Just False) mp
        (end, _) <- M.lookupMin $ M.filter (== Just True) mp
        walls <- NES.nonEmptySet . M.keysSet $ M.filter (== Nothing) mp
        let heur (p, d) =
              mannDist p end + case compare <$> p <*> end of
                V2 EQ EQ -> 0
                V2 EQ _ -> 0
                V2 _ EQ -> 0
                V2 _ _ -> 1000
            -- V2 EQ LT -> case d of
            --   North -> 0
            --   South -> 3000
            --   _ ->  2000
            -- V2 EQ GT -> case d of
            --   South -> 3000
            --   North -> 0
            --   _ ->  2000
            -- V2 LT  EQ -> case d of
            --     East -> 0
            --     West -> 3000
            --     _ -> 2000
            -- V2 GT EQ -> case d of
            --     West -> 0
            --     East -> 3000
            --     _ -> 2000
            -- V2 _ _ -> 1000
            step (p, d) =
              M.fromList
                [ ((p, d'), 1000)
                | d' <- [d <> West, d <> East]
                , (p + dirPoint d') `NES.notMember` walls
                ]
                <> if p' `NES.member` walls
                  then mempty
                  else M.singleton (p', d) 1
              where
                p' = p + dirPoint d
            proj (p, d) = (p, d `elem` [North, South])
        (_, ps) <- allMinimalPaths proj heur step (start, East) ((== end) . fst)
        pure . S.size $ (foldMap . foldMap) (S.singleton . fst) ps
        -- pure $ (foldMap . foldMap) (map . fst) ps
        -- costs = freqs . map fst $ aStarAll heur step (start, East) ((== end) . fst)
        -- costs = aStarAll heur step (start, East) ((== end) . fst)
        -- costs = aStarAll heur step (start, East) ((== end) . fst)
        -- Just bestPath = fst <$> aStar heur step (start, East) ((== end) . fst)
        -- goodPaths = go S.empty 0 (start, East)
        --   where
        --     go :: Set (Point, Dir) -> Int -> (Point, Dir) -> [Set Point]
        --     go seen score (p, d)
        --       | p == end = [S.singleton p | score == bestPath]
        --       | otherwise = do
        --           ((p', d'), cost) <- M.toList (step (p, d))
        --           guard $ cost + score <= bestPath
        --           guard $ (p', d') `S.notMember` seen
        --           guard $ cost + score + heur (p', d') <= bestPath
        --           S.insert p <$> go (S.insert (p, d) seen) (score + cost) (p', d')
        -- in -- allPaths = pathCost <$> bfsAll (M.keysSet . step) (start, East) ((== end) . fst)
        -- -- allPaths = filter (\p -> pathCost p == bestPath) $ bfsAll (M.keysSet . step) (start, East) ((== end) . fst)
        -- -- costs = M.fromListWith (<>) . map (second (S.fromList . map fst)) $ aStarAll heur step (start, East) ((== end) . fst)
        -- Just $ S.size $ fold goodPaths
        -- -- in S.size . S.fromList . map fst . snd <$> M.lookupMin costs
    }
  where
    pathCost xs = sum $ zipWith go xs (drop 1 xs)
      where
        go (p, d) (p', d')
          | p == p' = 1000
          | d == d' = 1
          | otherwise = undefined

-- solveB :: Point -> Point -> NESet Point -> Maybe _
-- solveB start end walls = Just $ M.fromList . mapMaybe (listToMaybe . G.unLPath) $ G.spTree startId mazeGraph
--   where
--     bb = boundingBox walls
--     graphNodes :: Map (Maybe (Point, Dir)) Int
--     graphNodes =
--       flip evalState 0 . sequenceA $
--         M.fromSet (\_ -> state \i -> (i, i + 1)) $
--           S.fromList $
--             Nothing
--               : [ Just (p, d)
--                 | p <- toList $ fillBoundingBox bb
--                 , p /= end
--                 , p `NES.notMember` walls
--                 , d <- [North ..]
--                 ]
--     startId :: Int
--     startId = graphNodes M.! Just (start, East)
--     graphEdges :: [G.LEdge Int]
--     graphEdges =
--       [ (i, graphNodes M.! pd', c)
--       | (Just (p, d), i) <- M.toList graphNodes
--       , let p' = p + dirPoint d
--       , (pd', c) <-
--           [ (Just (p, d <> West), 1000)
--           , (Just (p, d <> East), 100)
--           ]
--             ++ if
--               | p' == end -> [(Nothing, 1)]
--               | p' `NES.notMember` walls -> [(Just (p', d), 1)]
--               | otherwise -> []
--       ]
--     mazeGraph :: G.Gr (Maybe (Point, Dir)) Int
--     mazeGraph = G.mkGraph (swap <$> M.toList graphNodes) graphEdges
--     grabGoal :: [(Int, Int)] -> Maybe (Int, [Int])
--     grabGoal = \case
--       (0, c) : xs -> Just (c, fst <$> xs)
--       _ -> Nothing

-- (\_ -> state \i -> (i, i+1)) (fillBoundingBox bb)
-- mazeGraph :: G.Gr (Point, Dir) Int
-- mazeGraph = uncurry G.mkGraph $ flip evalStateT 0 $ _

-- data AMPState n p = AMPS
--   { _ampsClosed :: !(Map n (Maybe n))
--   -- ^ map of item to "parent"
--   , _ampsOpen :: !(OrdPSQ n p (p, Maybe n))
--   -- ^ map of item to "parent", and cost-so-far

data Path n m p = Path {pCurr :: n, pHistory :: [n], pSeen :: Set m, pCost :: p}
  deriving stock (Eq, Ord, Show)

allMinimalPaths ::
  forall n m p.
  (Ord n, Ord p, Num p, Show p, Show n, Ord m, Show m) =>
  -- | Duplicate projection
  (n -> m) ->
  -- | heuristic
  (n -> p) ->
  -- | neighborhood
  (n -> Map n p) ->
  -- | start
  n ->
  -- | target
  (n -> Bool) ->
  -- | all paths with the shortest cost
  Maybe (p, [[n]])
allMinimalPaths proj heur expand start targ = go0 (M.singleton start path0) (M.singleton (heur start) (NESeq.singleton path0))
  where
    path0 = Path start [start] S.empty 0
    go0 :: Map n (Path n m p) -> Map p (NESeq (Path n m p)) -> Maybe (p, [[n]])
    go0 bests queue = traceShow (M.size bests, sum (NESeq.length <$> queue)) do
      ((p, Path{..} NESeq.:<|| xs), queue') <- M.minViewWithKey queue
      let queue'' = case NESeq.nonEmptySeq xs of
            Nothing -> queue'
            Just xs' -> M.insert p xs' queue'
      if targ pCurr
        then traceShow p $ Just (p, pHistory : go1 p bests (M.takeWhileAntitone (<= p) queue''))
        else
          uncurry go0 . M.foldlWithKey' (processNeighbor pCost pHistory pSeen) (bests, queue'') $ expand pCurr
    go1 :: p -> Map n (Path n m p) -> Map p (NESeq (Path n m p)) -> [[n]]
    go1 minCost bests queue = case M.minViewWithKey queue of
      Nothing -> []
      Just ((p, Path{..} NESeq.:<|| xs), queue') ->
        let queue'' = case NESeq.nonEmptySeq xs of
              Nothing -> queue'
              Just xs' -> M.insert p xs' queue'
         in if targ pCurr
              then pHistory : go1 minCost bests queue''
              else
                uncurry (go1 minCost)
                  . second (M.takeWhileAntitone (<= minCost))
                  . M.foldlWithKey' (processNeighbor pCost pHistory pSeen) (bests, queue'')
                  $ expand pCurr
    processNeighbor ::
      p ->
      [n] ->
      Set m ->
      (Map n (Path n m p), Map p (NESeq (Path n m p))) ->
      n ->
      p ->
      (Map n (Path n m p), Map p (NESeq (Path n m p)))
    processNeighbor cost hist seen (bests, queue) x newCost
      | proj x `S.member` seen = (bests, queue)
      | otherwise = case M.lookup x bests of
          Nothing -> (M.insert x newPath bests, newQueue)
          Just Path{..}
            | cost + newCost <= pCost -> (M.insert x newPath bests, newQueue)
            | otherwise -> (bests, queue)
      where
        newPath = Path x (x : hist) (S.insert (proj x) seen) (cost + newCost)
        newQueue =
          M.insertWith
            (flip (<>))
            (cost + newCost + heur x)
            (NESeq.singleton newPath)
            queue

-- \| otherwise =

-- case M.lookupMin queue of
-- Nothing -> Nothing

-- where
-- go0 :: Seq (Path n p) -> Maybe (p, [[n]])
-- go0 queue = case S.
-- go :: Maybe Int -> Set (Path n0 n) -> Maybe (p, [[n]])

--   go :: Maybe p -> OrdPSQ n (Path n p)

-- solveB mp = Just 1
--   where
--     pathCost xs = sum $ zipWith go xs (drop 1 xs)
--       where
--         go (p, d) (p', d')
--           | p == p' = 1000
--           | d == d' = 1
--           | otherwise = undefined
--     start : _ = M.keys $ M.filter (== Just False) mp
--     end : _ = M.keys $ M.filter (== Just True) mp
--     walls = M.keysSet $ M.filter (== Nothing) mp
--     heur (p, d) =
--       mannDist p end + case compare <$> p <*> end of
--         V2 EQ EQ -> 0
--         V2 EQ _ -> 0
--         V2 _  EQ -> 0
--         V2 _ _ -> 1000
--     step (p, d) =
--       M.fromList
--         [ ((p, d'), 1000)
--         | d' <- [d <> East, d <> West]
--         ]
--         <> if (p + dirPoint d) `S.member` walls
--           then mempty
--           else M.singleton (p + dirPoint d, d) 1

insertIfBetter :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insertIfBetter k p x q = case Q.lookup k q of
  Nothing -> Q.insert k p x q
  Just (p', _)
    | p < p' -> Q.insert k p x q
    | otherwise -> q
