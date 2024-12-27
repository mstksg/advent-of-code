{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day21
where

-- (
-- day21a,
-- day21b,
-- dirPath,
-- composeDirPath,
-- composeDirPathLengths,
-- dirPathCosts,
-- runPath,
-- altP1,
-- )

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
import qualified Data.Vector.Sized as SV
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

-- okay i guess if we have a long series of bots, every time the top level
-- pushes A, an unbroken chain from the top also pushes A
--
-- Therefore any A cannot be pushed without the cascade of all the others
-- above it being pushed
--
-- This also means we have a "starting point" from which all memory is fresh.
--
-- The annoying thing is if there is more than one path, how can we really
-- know what is the best path to take?
--
-- Consider: moving from 0 to 6 to 9, vs 0 to 6 to 5. 06 could be ^^> or >^^,
-- but >^^ is better for moving to 9 later.
--
-- maybe is there a better A* heuristic for top-down? Instead of moving
-- randomly, move to a destination and then back. but then that's random.
--
-- Hm, okay maybe we can still go bottom-up: do things in triples instead of
-- pairs. Consider "0-6-9" to get to 6, then once you're at 6, consider
-- "6-9-A".
--
-- But now i wonder if the arrow keys need the same consideration? maybe
-- not...
--
-- Hmm yeah there is. Consider: A -> v -> <.
--
-- But actually wait would that ever matter? for the D pads you are always
-- going A to arrow to back, right? Could you optimize those? Yeah the D-pads
-- could be cached maybe: A-to-arrow-to-back?
--
-- Yeah in that case you always need to optimize only those 4: A^A, A>A, A<A,
-- AvA...
--
-- Okay here's the new goal: optimize all of the A-Dir-A marches at one level.
-- then two levels. then three levels. -> yeah this is already what we have
-- with the composeDirPath
--
-- For the final level we can then A-star again? or do something fundamentally
-- different.
--
-- We can at least compare with a ground truth A-star
--
-- 363987226123908 is too high
-- 360179530912464 is also too high
-- 352119886237752 is incorrect
-- 344484288881564 is incorrect
-- 411895844528756
--

type NumPad = Maybe (Finite 10)
type DirPad = Maybe Dir

-- 540A
-- 582A
-- 169A
-- 593A
-- 579A

pc :: Char -> Maybe (Finite 10)
pc = fmap fromIntegral . digitToIntSafe <=< mfilter isDigit . Just

applyPushDir :: Maybe Dir -> DirPad -> Maybe (DirPad, Maybe DirPad)
applyPushDir = \case
  Nothing -> \dp -> Just (dp, Just dp)
  Just North ->
    fmap (,Nothing) . \case
      Just South -> Just (Just North)
      Just East -> Just Nothing
      _ -> Nothing
  Just South ->
    fmap (,Nothing) . \case
      Just North -> Just (Just South)
      Nothing -> Just (Just East)
      _ -> Nothing
  Just East ->
    fmap (,Nothing) . \case
      Just North -> Just Nothing
      Just West -> Just (Just South)
      Just South -> Just (Just East)
      _ -> Nothing
  Just West ->
    fmap (,Nothing) . \case
      Nothing -> Just (Just North)
      Just South -> Just (Just West)
      Just East -> Just (Just South)
      _ -> Nothing

applyPushNum :: DirPad -> NumPad -> Maybe (NumPad, Maybe NumPad)
applyPushNum = \case
  Nothing -> \np -> Just (np, Just np)
  Just North ->
    fmap (,Nothing) . \case
      Just i
        | i /= 0 -> Just <$> packFinite (fromIntegral i + 3)
        | i == 0 -> Just (Just 2)
      Nothing -> Just (Just 3)
  Just South ->
    fmap (,Nothing) . \case
      Just i
        | i > 3 -> Just <$> packFinite (fromIntegral i - 3)
        | i == 3 -> Just Nothing
        | i == 2 -> Just (Just 0)
        | i == 1 -> Nothing
        | i == 0 -> Nothing
      _ -> Nothing
  Just East ->
    fmap (,Nothing) . \case
      Just i
        | i `elem` [3, 6, 9] -> Nothing
        | i == 0 -> Just Nothing
        | i /= 0 -> Just (Just (succ i))
      _ -> Nothing
  Just West ->
    fmap (,Nothing) . \case
      Just i
        | i `elem` [0, 1, 4, 7] -> Nothing
        | otherwise -> Just (Just (pred i))
      Nothing -> Just (Just 0)

data SearchState = SS
  { ssNumBot :: !NumPad
  , ssDirBot1 :: !DirPad
  , ssDirBot2 :: !DirPad
  , ssOutput :: !(Seq NumPad)
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)

findSol :: [NumPad] -> Maybe _
findSol goal = score . fst <$> aStar heur step s0 ((== goalseq) . ssOutput)
  where
    goalseq = Seq.fromList goal
    ngoal = length goal
    score p = p * read @Int (map intToDigit (mapMaybe (fmap fromIntegral) goal :: [Int]))
    heur SS{..} = ngoal - Seq.length ssOutput
    s0 =
      SS
        { ssNumBot = Nothing
        , ssDirBot1 = Nothing
        , ssDirBot2 = Nothing
        , ssOutput = mempty
        }
    step ss@SS{..} =
      M.fromSet (const 1) . S.fromList $
        [ SS{ssNumBot = numBot', ssDirBot1 = dirBot1', ssDirBot2 = dirBot2', ssOutput = output'}
        | push <- Nothing : (Just <$> [North ..])
        , (dirBot1', dbo1) <- maybeToList $ applyPushDir push ssDirBot1
        , (dirBot2', dbo2) <- case dbo1 of
            Nothing -> pure (ssDirBot2, Nothing)
            Just push' -> maybeToList $ applyPushDir push' ssDirBot2
        , (numBot', nbo) <- case dbo2 of
            Nothing -> pure (ssNumBot, Nothing)
            Just push' -> maybeToList $ applyPushNum push' ssNumBot
        , output' <- case nbo of
            Nothing -> pure ssOutput
            Just o -> do
              guard $ o == (goalseq `Seq.index` Seq.length ssOutput)
              pure (ssOutput Seq.:|> o)
        ]

day21a :: _ :~> _
day21a =
  MkSol
    { sParse = Just . map (map pc) . lines
    , -- noFail $
      --   lines
      -- , sShow = ('\n':) . unlines . map show . head
      sShow = show
    , -- , sSolve = fmap sum . traverse findSol
      sSolve =
        noFail $
          sum
            . map
              ( \p ->
                  let num :: Int
                      num = read (map intToDigit (mapMaybe (fmap fromIntegral) p :: [Int]))
                   in num * altP1' 2 p
              )
              -- , sSolve = fmap (fmap length) . traverse findSolBasic
              -- noFail $
              --   id
    }

data SearchStateN n = SSN
  { ssnNumBot :: !NumPad
  , ssnDirBots :: !(SV.Vector n DirPad)
  , ssnOutput :: !(Seq NumPad)
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)

findSolN :: [NumPad] -> Maybe _
findSolN goal = score . fst <$> aStar heur step s0 ((== goalseq) . ssnOutput)
  where
    goalseq = Seq.fromList goal
    ngoal = length goal
    score p = p * read @Integer (map intToDigit (mapMaybe (fmap fromIntegral) goal :: [Int]))
    heur SSN{..} = fromIntegral $ ngoal - Seq.length ssnOutput
    s0 :: SearchStateN 25
    s0 =
      SSN
        { ssnNumBot = Nothing
        , ssnDirBots = SV.replicate Nothing
        , ssnOutput = mempty
        }
    step ssn@SSN{..} =
      M.fromSet (const 1) . S.fromList $
        [ SSN{ssnNumBot = numBot', ssnDirBots = dirBots', ssnOutput = output'}
        | push <- Nothing : (Just <$> [North ..])
        , (dirBots', dbo) <- maybeToList $ flip runStateT (Just push) $ traverse pushDir1 ssnDirBots
        , -- scanlM pushDir1 _ ssnDirBots
        -- applyPushDirs push ssnDirBots
        (numBot', nbo) <- case dbo of
          Nothing -> pure (ssnNumBot, Nothing)
          Just push' -> maybeToList $ applyPushNum push' ssnNumBot
        , output' <- case nbo of
            Nothing -> pure ssnOutput
            Just o -> do
              guard $ o == (goalseq `Seq.index` Seq.length ssnOutput)
              pure (ssnOutput Seq.:|> o)
        ]

pushDir1 :: DirPad -> StateT (Maybe (Maybe Dir)) Maybe DirPad
pushDir1 bot = do
  currPush <- get
  case currPush of
    Nothing -> pure bot
    Just push -> do
      (bot', out) <- lift $ applyPushDir push bot
      put out
      pure bot'

allDirPad :: [DirPad]
allDirPad = [Nothing, Just East, Just North, Just South, Just West]

class Ord a => Pushable a where
  allPushable :: [a]
  applyPush :: DirPad -> Maybe a -> Maybe (Maybe a, Maybe (Maybe a))

allPushable' :: Pushable a => [Maybe a]
allPushable' = Nothing : fmap Just allPushable

instance Pushable Dir where
  allPushable = [West, North, South, East]
  applyPush = applyPushDir

instance Pushable (Finite 10) where
  allPushable = finites
  applyPush = applyPushNum

-- | Best way to get from button to button. penalize motion
dirPath :: forall a. Pushable a => Map (Maybe a) (Map (Maybe a) [DirPad])
dirPath = M.fromSet ((`M.fromSet` S.fromList allPushable') . go) (S.fromList allPushable')
  where
    -- go :: Maybe a -> Maybe a -> [DirPad]
    -- go x y = (++ [Nothing]) . fromJust $ bfsActions step x (== y)
    --   where
    --     step p =
    --       [ (d, p')
    --       | d <- [Just West, Just North, Just South, Just East]
    --       , (p', Nothing) <- maybeToList $ applyPush d p
    --       ]
    go :: Maybe a -> Maybe a -> [DirPad]
    go x y = runPath Nothing . fromJust $ bfsActions step (Left (x, Nothing)) (== Right y)
      where
        step (Left (b, d)) =
          reverse
            [ ( push
              , case bout of
                  Nothing -> Left (b', d')
                  Just o -> Right o
              )
            | push <- toList allDirPad
            , (d', dout) <- maybeToList $ applyPush push d
            , (b', bout) <- case dout of
                Nothing -> pure (b, Nothing)
                Just push' -> maybeToList $ applyPush push' b
            ]
        step (Right _) = []

-- -- | Best way to get from button to button
-- dirPath :: forall a. Pushable a => Map (Maybe a) (Map (Maybe a) [DirPad])
-- dirPath = M.fromSet ((`M.fromSet` allPushable') . go) allPushable'
--   where
--     go :: Maybe a -> Maybe a -> [DirPad]
--     go x y = fromJust $ bfsActions step (Left x) (== Right y)
--       where
--         step (Left d) =
--           M.fromList
--             [ case dout of
--                 Nothing -> (push, Left d')
--                 Just o -> (push, Right o)
--             | push <- toList allDirPad
--             , (d', dout) <- maybeToList $ applyPush push d
--             ]
--         step (Right _) = M.empty

-- yeah I guess at each up/down step is independent of each other
dirPathCosts :: Pushable a => Map (Maybe a) (Map (Maybe a) Int)
dirPathCosts = (fmap . fmap) length dirPath

-- | missing the first element
spellDirPath ::
  Ord a =>
  Map (Maybe a) (Map (Maybe a) [Maybe b]) ->
  [Maybe a] ->
  [Maybe b]
spellDirPath mp xs = concat $ zipWith (\x y -> (mp M.! x) M.! y) xs (drop 1 xs)

-- composeDirPath ::
--   (Ord a, Pushable b) =>
--   Map (Maybe b) (Map (Maybe b) [Maybe c]) ->
--   Map (Maybe a) (Map (Maybe a) [Maybe b]) ->
--   Map (Maybe a, Maybe b) (Map (Maybe a) [Maybe c])
-- composeDirPath mpBC mpAB = M.fromListWith M.union
--   [ ((a0, b0), M.singleton a1 $ (spellDirPath mpBC $ b0 : pathA))
--     | (a0, as) <- M.toList mpAB
--     , (a1, pathA) <- M.toList as
--     ,  b0 <- toList allPushable'
--     -- , (b0, bs) <- M.toList mpBC
--     -- , (b1, pathB) <- M.toList bs
--   ]
--   -- (fmap . fmap) (spellDirPath mp undefined)

-- | this seems to work but at n=18 we get to 200,000,000 ... this grows too
-- big to keep them all in memory i think. maybe just keep the lengths?
composeDirPath ::
  Ord b =>
  Map (Maybe b) (Map (Maybe b) [Maybe c]) ->
  Map (Maybe a) (Map (Maybe a) [Maybe b]) ->
  Map (Maybe a) (Map (Maybe a) [Maybe c])
composeDirPath mp = (fmap . fmap) (spellDirPath mp . (Nothing :))

-- | missing the first element
spellDirPathLengths ::
  Ord a =>
  Map (Maybe a) (Map (Maybe a) Int) ->
  [Maybe a] ->
  Int
spellDirPathLengths mp xs = sum $ zipWith (\x y -> (mp M.! x) M.! y) xs (drop 1 xs)

-- | this seems to work but at n=18 we get to 200,000,000 ... this grows too
-- big to keep them all in memory i think. maybe just keep the lengths?
composeDirPathLengths ::
  Ord b =>
  Map (Maybe b) (Map (Maybe b) Int) ->
  Map (Maybe a) (Map (Maybe a) [Maybe b]) ->
  Map (Maybe a) (Map (Maybe a) Int)
composeDirPathLengths mp = (fmap . fmap) (spellDirPathLengths mp . (Nothing :))

-- [Just South,Just West,Nothing,Just West,Nothing,Nothing,Just East,Just
-- North,Just East,Nothing,Just South,Nothing,Just North,Just
-- West,Nothing,Just East,Just South,Nothing,Just North,Nothing]
-- v<A<AA>^>AvA^<A>vA^A
-- [Just South,Just West,Just West,Nothing,Just East,Just North,Just East,Nothing]
-- v<<A>^>A
-- ah hah, that's the key! >^> is more costly than >>^
-- we must penalize changes

-- <vA<AA>>^AvAA<^A>A
--   v <<   A >>  ^ A
--    <   A
-- 029A

runPath :: Pushable a => Maybe a -> [DirPad] -> [Maybe a]
runPath x = \case
  [] -> []
  d : ds -> case applyPush d x of
    Nothing -> error $ "hm..." ++ show d
    Just (y, out) -> maybe id (:) out $ runPath y ds

-- altP1 :: [NumPad] -> Int
-- altP1 = length . spellDirPath mp
--   where
--     mp = dirPath @Dir `composeDirPath` dirPath @Dir `composeDirPath` dirPath @(Finite 10)

altP1 :: Int -> [NumPad] -> Int
altP1 n = spellDirPathLengths mp . (Nothing :)
  where
    mpChain :: [Map DirPad (Map DirPad Int)]
    mpChain = iterate (`composeDirPathLengths` dirPath @Dir) (dirPathCosts @Dir)
    mp = (mpChain !! (n - 1)) `composeDirPathLengths` dirPath @(Finite 10)

altP1' :: Int -> [NumPad] -> Int
altP1' n ps = minimum do
  npp <- toList $ fullPadPaths (Nothing : ps)
  -- dpp <- toList $ fullPadPaths (Nothing : npp)
  -- dpp' <- toList $ fullPadPaths (Nothing : dpp)
  pure $ spellDirPathLengths mp (Nothing : npp)
  where
    mpChain :: [Map DirPad (Map DirPad Int)]
    mpChain = iterate (`composeDirPathLengths` dirPath @Dir) (dirPathCosts @Dir)
    mp = mpChain !! (n - 1)

altP1'' :: Int -> [NumPad] -> Int
altP1'' n ps = minimum do
  npp <- traceLength . toList $ fullPadPaths (Nothing : ps)
  traceM $ "npp " <> show npp
  dpp <- traceLength . toList $ fullPadPaths (Nothing : npp)
  traceM $ "dpp " <> show dpp
  dpp' <- traceLength . toList $ fullPadPaths (Nothing : dpp)
  traceM $ "dpp' " <> show dpp'
  pure $ length dpp'
  where
    traceLength xs = traceShow (length xs) xs

findFixed :: Pushable a => Maybe a -> Maybe a -> _
findFixed a b = fst $ minimumBy (comparing length) do
  npp <- toList $ padPaths a b
  dpp <- toList $ fullPadPaths (Nothing : npp)
  dpp' <- toList $ fullPadPaths (Nothing : dpp)
  dpp'' <- toList $ fullPadPaths (Nothing : dpp')
  -- dpp''' <- toList $ fullPadPaths (Nothing : dpp'')
  pure (npp, length dpp'')
  where
    -- npp <- traceLength . toList $ fullPadPaths (Nothing : ps)
    -- traceM $ "npp " <> show npp
    -- dpp <- traceLength . toList $ fullPadPaths (Nothing : npp)
    -- traceM $ "dpp " <> show dpp
    -- dpp' <- traceLength . toList $ fullPadPaths (Nothing : dpp)
    -- traceM $ "dpp' " <> show dpp'
    -- pure $ length dpp'

    traceLength xs = traceShow (length xs) xs

-- pure $ spellDirPathLengths mp (Nothing:npp)
-- where
-- mpChain :: [Map DirPad (Map DirPad Int)]
-- mpChain = iterate (`composeDirPathLengths` dirPath @Dir) (dirPathCosts @Dir)
-- mp = mpChain !! (n - 1)

-- applyPush :: DirPad -> Maybe a -> Maybe (Maybe a, Maybe (Maybe a))

---- | Best way to get from button to button
----
---- Assume that same-to-same means same-to-A
-- dirPath :: forall a. Pushable a => Map (Maybe a) (Map (Maybe a) [_])
-- dirPath = M.fromSet ((`M.fromSet` allPushable') . go) allPushable'
--  where
--    go :: Maybe a -> Maybe a -> [DirPad]
--    go x y = mapMaybe (preview (_Left . _1)) . fromJust $ bfs step (Left (Nothing, x)) (== Right y)
--      where
--        step (Left (d, b)) = S.fromList
--          [ case bout of
--              Nothing -> Left (d', b')
--              Just o -> Right o
--            | push <- toList allDirPad
--          , (d', dout) <- maybeToList $ applyPushDir push d
--          , (b', bout) <- case dout of
--                          Nothing -> pure (b, Nothing)
--                          Just push' -> maybeToList $ applyPush push' b
--          ]
--        step (Right _) = S.empty

-- applyPushNum :: DirPad -> NumPad -> Maybe (NumPad, Maybe NumPad)
-- applyPushDir :: Maybe Dir -> DirPad -> Maybe (DirPad, Maybe DirPad)
-- step (Left (d, b)) = S.fromList
--   [ case bout of
--       Nothing -> Left (d', b')
--       Just o -> Right o
--     | push <- toList allDirPad
--   , (d', dout) <- maybeToList $ applyPushDir push d
--   , (b', bout) <- case dout of
--                   Nothing -> pure (b, Nothing)
--                   Just push' -> maybeToList $ applyPushDir push' b

--   ]
-- step (Right _) = S.empty
--
-- new realization, only the "top"

day21b :: _ :~> _
day21b =
  MkSol
    { sParse = sParse day21a
    , sShow = show
    , sSolve =
        noFail $
          sum
            . map
              ( \p ->
                  let num :: Int
                      num = read (map intToDigit (mapMaybe (fmap fromIntegral) p :: [Int]))
                   in num * altP1 25 p
              )
    }

numPadPaths :: NumPad -> NumPad -> Set [DirPad]
numPadPaths start goal = fromMaybe S.empty do
  minLen <- minimumMay $ length <$> options
  pure $ S.fromList $ filter ((== minLen) . length) options
  where
    options = go S.empty start
    go seen p = do
      guard $ p `S.notMember` seen
      d <- allDirPad
      -- Nothing : (Just <$> [North ..])
      (p', o) <- maybeToList $ applyPushNum d p
      (d :) <$> case o of
        Nothing -> go (S.insert p seen) p'
        Just o' -> if o' == goal then pure [] else empty

fullNumPadPaths :: [NumPad] -> Set [DirPad]
fullNumPadPaths xs = S.fromList $ concat <$> zipWithM (\a b -> toList $ numPadPaths a b) xs (drop 1 xs)

-- | a lot of these can be pruned waay by getting rid of NEN/ENE etc.
padPaths :: Pushable a => Maybe a -> Maybe a -> Set [DirPad]
padPaths start goal = fromMaybe S.empty do
  minLen <- minimumMay $ length <$> options
  pure $ S.fromList $ filter ((== minLen) . length) options
  where
    options = go S.empty start
    go seen p = do
      guard $ p `S.notMember` seen
      d <- allDirPad
      -- Nothing :
      -- (Just <$> [North ..])
      (p', o) <- maybeToList $ applyPush d p
      (d :) <$> case o of
        Nothing -> go (S.insert p seen) p'
        Just o' -> if o' == goal then pure [] else empty

fullPadPaths :: Pushable a => [Maybe a] -> Set [DirPad]
fullPadPaths xs = S.fromList $ concat <$> zipWithM (\a b -> toList $ padPaths a b) xs (drop 1 xs)

-- | Best way to get from button to button.  Ignore third path.
--
-- Hmm yeah we need to somehow involve the triple here otherwise no changes
-- propagate
--
-- We probably have to generate this as we go
dirPathTriples :: forall a. Pushable a => Map (Maybe a) (Map (Maybe a) (Map (Maybe a) [DirPad]))
dirPathTriples = fmap (\xs -> M.fromSet (const xs) $ S.fromList allPushable') <$> dirPath @a

-- | Only from X to Y, does not include from Y to Z
composeTriples ::
  Ord b =>
  Map (Maybe b) (Map (Maybe b) (Map (Maybe b) [Maybe c])) ->
  Map (Maybe a) (Map (Maybe a) (Map (Maybe a) [Maybe b])) ->
  Map (Maybe a) (Map (Maybe a) (Map (Maybe a) [Maybe c]))
composeTriples mp = (fmap . fmap . fmap) (spellTriples mp . (Nothing :))

-- | missing the first element
spellTriples ::
  Ord a =>
  Map (Maybe a) (Map (Maybe a) (Map (Maybe a) [Maybe b])) ->
  [Maybe a] ->
  [Maybe b]
spellTriples mp xs = concat $ zipWith3 (\x y z -> ((mp M.! x) M.! y) M.! z) xs (drop 1 xs) (drop 2 xs)

-- Map (Maybe b) (Map (Maybe b) [Maybe c]) ->
-- Map (Maybe a) (Map (Maybe a) [Maybe b]) ->
-- Map (Maybe a) (Map (Maybe a) [Maybe c])

-- score p = p * read @Int (map intToDigit (mapMaybe (fmap fromIntegral) goal :: [Int]))

data BotState a = BS
  { bsCache :: Map (V3 (Maybe a)) Int
  , bsCurr :: Maybe a
  , bsHistory :: Maybe (Maybe a, Maybe (Maybe a))
  }

-- | How do we even step once? I guess we can search, 2^25 is the maximum
-- bound pretty much
stepBotState :: BotState Dir -> BotState Dir
stepBotState = undefined

-- data BotChain = BCNil NumPad

-- requestMotion :: NumPad -> State (NumPad, [DirPad]) Int
-- requestMotion = undefined
