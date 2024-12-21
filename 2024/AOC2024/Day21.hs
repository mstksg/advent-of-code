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
module AOC2024.Day21 (
  day21a,
  day21b,
  dirPath,
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
import qualified Data.Vector.Sized as SV
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

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
    , sSolve = fmap sum . traverse findSol
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
      traceShow ssn $
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
                traceM $ show (ssnOutput Seq.:|> o)
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

allDirPad :: Set DirPad
allDirPad = S.fromList $ Nothing : (Just <$> [North ..])

class Ord a => Pushable a where
  allPushable :: Set a
  applyPush :: DirPad -> Maybe a -> Maybe (Maybe a, Maybe (Maybe a))

allPushable' :: Pushable a => Set (Maybe a)
allPushable' = S.insert Nothing (S.mapMonotonic Just allPushable)

instance Pushable Dir where
  allPushable = S.fromList [North ..]
  applyPush = applyPushDir

instance Pushable (Finite 10) where
  allPushable = S.fromList finites
  applyPush = applyPushNum

-- | Best way to get from button to button
dirPath :: forall a. Pushable a => Map (Maybe a) (Map (Maybe a) [DirPad])
dirPath = M.fromSet ((`M.fromSet` allPushable') . go) allPushable'
  where
    go :: Maybe a -> Maybe a -> [DirPad]
    go x y = fromJust $ bfsActions step (Left x) (== Right y)
      where
        step (Left d) =
          M.fromList
            [ case dout of
                Nothing -> (push, Left d')
                Just o -> (push, Right o)
            | push <- toList allDirPad
            , (d', dout) <- maybeToList $ applyPush push d
            ]
        step (Right _) = M.empty

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

day21b :: _ :~> _
day21b =
  MkSol
    { sParse = sParse day21a
    , sShow = show
    , sSolve = fmap sum . traverse findSolN
    }
