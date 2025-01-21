{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- |
-- Module      : AOC2024.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day24 (
  day24a,
  day24b,
)
where

import AOC.Common (asString, loopEither, parseBinary)
import AOC.Common.Parser (CharParser, pAlphaNumWord, parseMaybe', sepByLines, tokenAssoc)
import AOC.Prelude
import AOC.Solver (noFail, type (:~>) (..))
import Control.Applicative (Alternative (empty, many))
import Control.DeepSeq (NFData)
import Control.Lens ((%=))
import Control.Monad.Free (Free, MonadFree (wrap), iterA)
import Control.Monad.Logic (LogicT, MonadLogic (interleave), observeT)
import Control.Monad.State (MonadState (get, put), State, StateT, execState, execStateT)
import Data.Bifunctor (Bifunctor (second))
import Data.Bitraversable
import Data.Finite (strengthenN, weaken, weakenN)
import Data.Foldable (Foldable (toList))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive as G
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (intercalate, isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.Type.Equality
import GHC.Generics (Generic)
import GHC.TypeNats
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Printf (printf)

data Op = OAnd | OOr | OXor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data InVar = IVX Int | IVY Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- newtype OutVar n = IVZ (Finite (n + 1))

-- data Var :: Nat -> Type where
--   VX :: Finite n -> Var n
--   VY :: Finite n -> Var n
--   VZ :: Finite (n + 1) -> Var n
--   deriving stock (Eq, Ord, Show, Generic)
--   deriving anyclass (NFData)

data GateName = GNInVar InVar | GNOutVar Int | GNString Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data Gate a = Gate {gOp :: Op, gX :: a, gY :: a}
  deriving stock (Show, Generic, Functor, Traversable, Foldable)
  deriving anyclass (NFData)

normalizeGate :: Ord a => Gate a -> Gate a
normalizeGate (Gate o x y)
  | x <= y = Gate o x y
  | otherwise = Gate o y x

instance Ord a => Eq (Gate a) where
  a == b = case (normalizeGate a, normalizeGate b) of
    (Gate o x y, Gate o' x' y') -> o == o' && x == x' && y == y'

instance Ord a => Ord (Gate a) where
  compare a b = case (normalizeGate a, normalizeGate b) of
    (Gate o x y, Gate o' x' y') -> mconcat [compare o o', compare x x', compare y y']

-- parseFinite :: KnownNat m => CharParser (Finite m)
-- parseFinite =
--   pDecimal >>= \i -> case packFinite i of
--     Nothing -> fail "n out of range"
--     Just x -> pure x

parseInVar :: CharParser InVar
parseInVar = P.choice ["x" >> IVX <$> pDecimal, "y" >> IVY <$> pDecimal]

parseOutVar :: CharParser Int
parseOutVar = "z" >> pDecimal

parseGateName :: CharParser GateName
parseGateName = GNInVar <$> parseInVar <|> GNOutVar <$> parseOutVar <|> GNString . T.pack <$> pAlphaWord

parseGate :: CharParser (Gate GateName)
parseGate = do
  gX <- parseGateName <* pSpace
  gOp <-
    P.choice
      [ OAnd <$ "AND"
      , OOr <$ "OR"
      , OXor <$ "XOR"
      ]
      <* pSpace
  gY <- parseGateName
  pure Gate{..}

parseInitial :: CharParser (InVar, Bool)
parseInitial = (,) <$> parseInVar <* ": " <*> tokenAssoc [('0', False), ('1', True)]

applyOp :: Op -> Bool -> Bool -> Bool
applyOp = \case
  OAnd -> (&&)
  OOr -> (||)
  OXor -> (/=)

applyGate :: Gate Bool -> Bool
applyGate Gate{..} = applyOp gOp gX gY

day24a :: ([(InVar, Bool)], [(Gate GateName, GateName)]) :~> _
day24a =
  MkSol
    { sParse = parseMaybe' do
        cs <- P.many $ parseInitial <* P.newline
        P.newline
        os <- sepByLines $ (,) <$> parseGate <* "-> " <*> parseGateName
        pure (cs, os)
    , sShow = show
    , sSolve =
        noFail \(inputs, gates) ->
          sum . map (2 ^) . toList $
            runRules (M.fromList $ swap <$> gates) (M.fromList inputs)
    }

runRules ::
  Map GateName (Gate GateName) ->
  Map InVar Bool ->
  Set Int
runRules gates inputs = S.fromDistinctAscList [n | (GNOutVar n, True) <- M.toAscList results]
  where
    results = M.mapKeysMonotonic GNInVar inputs <> (applyGate . fmap (results M.!) <$> gates)

-- runGraph :: [G.Node] -> G.Gr Op () -> [Bool]
-- runGraph = G.gfold G.pre' apGate (maybe id (:), [])
--   where
--     apGate :: G.Context Op () -> [Bool] -> Bool
--     apGate (_, _, o, _) = foldr1 (applyOp o)

-- results <- M.mapKeysMonotonic GNInVar inputs <> (f . fmap (results M.!) <$> gates)
--   S.fromDistinctAscList [n | (GNOutVar n, True) <- M.toAscList results]
-- where
-- results = M.mapKeysMonotonic GNInVar inputs <> (f . fmap (results M.!) <$> gates)

-- -- | Restrict the number of gates
-- strengthenGateName :: GateName -> Maybe GateName
-- strengthenGateName = \case
--   GNInVar (IVX x) -> GNInVar . IVX <$> strengthenN x
--   GNInVar (IVY y) -> GNInVar . IVY <$> strengthenN y
--   GNOutVar z -> GNOutVar <$> strengthenN z
--   GNString l -> Just $ GNString l

-- weakenSwappable :: Swappable n -> Swappable n
-- weakenSwappable = \case
--   SOutVar z -> SOutVar (weakenN z)
--   SString l -> SString l

_Swappable :: Prism' GateName Swappable
_Swappable = prism' unSwap toSwap
  where
    unSwap = \case
      SOutVar z -> GNOutVar z
      SString t -> GNString t
    toSwap = \case
      GNInVar _ -> Nothing
      GNOutVar z -> Just $ SOutVar z
      GNString t -> Just $ SString t

data Swappable = SOutVar Int | SString Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

showSwappable :: Swappable -> String
showSwappable = \case
  SOutVar n -> 'z' : printf "%02d" n
  SString t -> T.unpack t

rulesGraph :: Map GateName (Gate GateName) -> G.Gr GateName Op
rulesGraph rules =
  G.mkGraph
    (zip [0 ..] (M.keys rules))
    [ (i, j, o)
    | (i, Gate o x y) <- zip [0 ..] $ M.elems rules
    , j <- mapMaybe (`M.lookupIndex` rules) [x, y]
    ]

traceBack :: Map GateName (Gate GateName) -> Set Swappable -> Set Swappable
traceBack rules = go S.empty
  where
    go seen edge
      | S.null edge' = seen'
      | otherwise = go seen' edge'
      where
        seen' :: Set Swappable
        seen' = seen <> edge
        edge' :: Set Swappable
        edge' =
          foldMap
            (\e -> S.fromList . mapMaybe (preview _Swappable) . toList $ rules M.! review _Swappable e)
            edge
            `S.difference` seen'

traceBack' :: Map GateName (Gate GateName) -> Set GateName -> Set GateName
traceBack' rules = go S.empty
  where
    go seen edge
      | S.null edge' = seen'
      | otherwise = go seen' edge'
      where
        seen' = seen <> edge
        edge' = foldMap (\e -> S.fromList . toList $ rules M.! e) edge `S.difference` seen'

-- fromList [(SString "bjm",SString "kbk"),(SString "kbk",SString "bjm")]
-- fromList [(SString "bjm",SString "njc"),(SString "njc",SString "bjm")]
-- fromList [(SString "cbn",SString "kbk"),(SString "kbk",SString "cbn")]
-- fromList [(SString "kbk",SString "mkm"),(SString "mkm",SString "kbk")]
--
-- Actual swaps:
-- [("bjm","z07")
-- ,("hsw","z13")
-- ,("nvr","wkr")
-- ,("skf","z18")
-- ,("wkr","nvr")
-- ,("z07","bjm")
-- ,("z13","hsw")
-- ,("z18","skf")]
--
-- [("bjm","z07")
-- ,("hsw","z13")
-- ,("nvr","wkr")
-- ,("skf","z18")
-- ]

-- okay let's try: we can count the layers of every new swap

shells :: Map GateName (Gate GateName) -> [Set Swappable]
shells rules = unfoldr expand (0, S.empty)
  where
    expand :: (Int, Set Swappable) -> Maybe (Set Swappable, (Int, Set Swappable))
    expand (n, seen)
      | n == 46 = Nothing
      | otherwise =
          Just
            let newShell = traceBack rules (S.singleton (SOutVar n))
             in (newShell `S.difference` seen, (n + 1, newShell `S.union` seen))

-- fromList [SOutVar 0]
-- fromList [SOutVar 1,SString "ghh",SString "gnn"]
-- fromList [SOutVar 2,SString "fwk",SString "msp",SString "nck",SString "qhq"]
-- fromList [SOutVar 3,SString "gmp",SString "gpn",SString "rgn",SString "vbs"]
-- fromList [SOutVar 4,SString "djp",SString "dvh",SString "mpq",SString "wwt"]
-- fromList [SOutVar 5,SString "qgg",SString "sws",SString "tpq",SString "vds"]
-- fromList [SOutVar 6,SString "fsw",SString "hjv",SString "nhj",SString "vwc"]
-- fromList [SOutVar 7,SString "cbn",SString "dvw",SString "kbk",SString "mkm",SString "njc",SString "tgj"]
-- fromList [SOutVar 8,SString "bjm",SString "btr"]
-- fromList [SOutVar 9,SString "bsd",SString "fvm",SString "gdw",SString "tns"]
-- fromList [SOutVar 10,SString "ckp",SString "dvd",SString "kgf",SString "rds"]
-- fromList [SOutVar 11,SString "gwd",SString "pjt",SString "sbg",SString "spf"]
-- fromList [SOutVar 12,SString "ggj",SString "hcv",SString "pnq",SString "wpw"]
-- fromList [SOutVar 13,SString "pmv",SString "rbk",SString "rtn",SString "wrg"]
-- fromList [SOutVar 14,SString "bpf",SString "hsw",SString "vhb",SString "vjf"]
-- fromList [SOutVar 15,SString "dtw",SString "rmm",SString "tgb",SString "tst"]
-- fromList [SOutVar 16,SString "bsc",SString "fcf",SString "gpt",SString "gqg"]
-- fromList [SOutVar 17,SString "bmg",SString "fjf",SString "nbp",SString "pnh"]
-- fromList [SOutVar 18]
-- fromList [SOutVar 19,SString "bgt",SString "gms",SString "jct",SString "qjc",SString "rrq",SString "scd",SString "skf",SString "tgm"]
-- fromList [SOutVar 20,SString "bqj",SString "kgh",SString "mwk",SString "rfg"]
-- fromList [SOutVar 21,SString "bcv",SString "pnb",SString "swd",SString "tch"]
-- fromList [SOutVar 22,SString "djg",SString "dnk",SString "drk",SString "vfr"]
-- fromList [SOutVar 23,SString "hkk",SString "tdg",SString "tmq",SString "vft"]
-- fromList [SOutVar 24,SString "hqw",SString "knh",SString "wbw",SString "wjw"]
-- fromList [SOutVar 25,SString "dcn",SString "pds",SString "qmb",SString "wsg"]
-- fromList [SOutVar 26,SString "kvp",SString "qkf",SString "qsm",SString "wkr"]
-- fromList [SOutVar 27,SString "bpb",SString "crd",SString "frt",SString "nvr"]
-- fromList [SOutVar 28,SString "hcb",SString "qcg",SString "rwm",SString "vvc"]
-- fromList [SOutVar 29,SString "cdj",SString "gph",SString "tmp",SString "tvw"]
-- fromList [SOutVar 30,SString "cmv",SString "fdb",SString "smd",SString "sss"]
-- fromList [SOutVar 31,SString "djm",SString "dkc",SString "sst",SString "tjh"]
-- fromList [SOutVar 32,SString "bkc",SString "fwv",SString "mqr",SString "rwk"]
-- fromList [SOutVar 33,SString "jjb",SString "rhk",SString "vkw",SString "wjm"]
-- fromList [SOutVar 34,SString "hmm",SString "mwg",SString "pcn",SString "vbm"]
-- fromList [SOutVar 35,SString "bfg",SString "fnf",SString "mhg",SString "tdd"]
-- fromList [SOutVar 36,SString "cck",SString "fjm",SString "gtk",SString "sfj"]
-- fromList [SOutVar 37,SString "bkm",SString "djd",SString "fjq",SString "vnm"]
-- fromList [SOutVar 38,SString "bht",SString "fvp",SString "gvr",SString "jkm"]
-- fromList [SOutVar 39,SString "pth",SString "qrb",SString "shn",SString "vjb"]
-- fromList [SOutVar 40,SString "cvf",SString "jmc",SString "pmm",SString "tgv"]
-- fromList [SOutVar 41,SString "cgh",SString "cjs",SString "hfj",SString "vkd"]
-- fromList [SOutVar 42,SString "fvk",SString "pbn",SString "rmv",SString "spd"]
-- fromList [SOutVar 43,SString "gpv",SString "jrk",SString "ngt",SString "whv"]
-- fromList [SOutVar 44,SString "jkh",SString "mbw",SString "tcb",SString "wkd"]
-- fromList [SOutVar 45,SString "dwj",SString "qvk"]

isOutVar :: Swappable -> Bool
isOutVar (SOutVar _) = True
isOutVar (SString _) = False

alignShell ::
  Int ->
  Map GateName (Gate GateName) ->
  [(Maybe (Swappable, Swappable), Map GateName (Gate GateName))]
alignShell n rules
  | n <= 2 = [(Nothing, rules)]
  | validateSwaps rules = [(Nothing, rules)]
  | otherwise = do
      swap0 <- S.toList $ shellThere `S.difference` shellHere
      swap1 <-
        S.toList $
          S.fromDistinctAscList (mapMaybe (preview _Swappable) $ M.keys rules)
            `S.difference` shellBack
      let rules' =
            M.insert (review _Swappable swap1) (rules M.! review _Swappable swap0)
              . M.insert (review _Swappable swap0) (rules M.! review _Swappable swap1)
              $ rules
          gr = rulesGraph rules'
      guard $ not (G.hasLoop gr) && all ((<= 1) . length) (G.scc gr)
      guard $ validateSwaps rules'
      pure (Just (swap0, swap1), rules')
  where
    shellBack = traceBack rules (S.fromDistinctAscList . map SOutVar $ take (n - 1) [0 ..])
    shellHere = traceBack rules (S.fromDistinctAscList . map SOutVar $ take n [0 ..])
    shellThere = traceBack rules (S.fromDistinctAscList . map SOutVar $ take (n + 1) [0 ..])
    validateSwaps :: Map GateName (Gate GateName) -> Bool
    validateSwaps swappedRules = and do
      x0 <- [False, True]
      y0 <- [False, True]
      x1 <- [False, True]
      y1 <- [False, True]
      let inpMap =
            M.union
              ( M.fromList
                  [ (IVX (n - 2), x0)
                  , (IVY (n - 2), y0)
                  , (IVX (n - 1), x1)
                  , (IVY (n - 1), y1)
                  ]
              )
              $ M.fromList [(mk v, False) | mk <- [IVX, IVY], v <- [0 .. 44]]
          expected =
            S.fromList . catMaybes $
              [ (n - 2) <$ guard (x0 /= y0)
              , (n - 1) <$ guard ((x1 /= y1) /= (x0 && y0))
              , n <$ guard ((x1 && y1) || ((x0 && y0) && (x1 /= y1)))
              ]
          result = runRules swappedRules inpMap
      pure $ result == expected

alignShells :: Map GateName (Gate GateName) -> [Map Swappable Swappable]
alignShells = go M.empty 0
  where
    go :: Map Swappable Swappable -> Int -> Map GateName (Gate GateName) -> [Map Swappable Swappable]
    -- go (traceShowId -> (!outs)) (traceShowId -> n) rules
    go outs n rules
      | n == 46 = [outs]
      | otherwise = do
          (newSwap, rules') <- alignShell n rules
          let outs' = maybe id (\(x, y) -> M.insert x y . M.insert y x) newSwap outs
          guard $ M.size outs' < 9
          go outs' (n + 1) rules'

findSwaps ::
  Int ->
  -- | do not change
  Set Swappable ->
  Map Swappable Swappable ->
  Map GateName (Gate GateName) ->
  Maybe (Map Swappable Swappable)
findSwaps (traceShowId -> n) noswap swaps gates
  | n == 7 = Just swaps
  | validateSwaps gates = findSwaps (n + 1) (traceBack gates (S.fromList []) <> noswap) swaps gates
  | otherwise = do
      (gates', goodSwap) <-
        listToMaybe . (\q -> trace (show (snd <$> q)) q) . filter (validateSwaps . fst) $ do
          -- listToMaybe . (\q -> trace ("length " <> show (length q)) q) . filter (validateSwaps . fst) $ do
          swap1 : rest <- tails allCandidates
          guard $ swap1 `M.notMember` swaps
          guard $ swap1 `S.notMember` noswap
          swap2 <- rest
          guard $ swap2 `M.notMember` swaps
          guard $ swap2 `S.notMember` noswap
          let swaps' = traceShowId $ M.fromList [(swap1, swap2), (swap2, swap1)]
              swappedRules =
                M.insert (review _Swappable swap2) (gates M.! review _Swappable swap1)
                  . M.insert (review _Swappable swap1) (gates M.! review _Swappable swap2)
                  $ gates
              -- fmap (applySwap swaps') <$> gates
              gr = rulesGraph swappedRules
          -- journey = G.rdfs [z | Just z <- (`M.lookupIndex` swappedRules) . GNOutVar <$> [n, n + 1]] gr
          -- roots = mapMaybe (preview _Swappable . fst . (`M.elemAt` swappedRules)) journey
          -- traceM $ show . sort . map length . G.scc $ rulesGraph swappedRules
          guard $ not (G.hasLoop gr) && all ((<= 1) . length) (G.scc gr)
          -- guard $ all ((<= 1) . length) . G.scc $ rulesGraph swappedRules
          pure (swappedRules, swaps')
      findSwaps (n + 1) (traceBack gates' (S.fromList []) <> noswap) (goodSwap <> swaps) gates'
  where
    applySwap swapMap = over _Swappable \q -> M.findWithDefault q q swapMap
    allCandidates :: [Swappable]
    allCandidates = mapMaybe (preview _Swappable) $ M.keys gates
    validateSwaps :: Map GateName (Gate GateName) -> Bool
    validateSwaps swappedRules = and do
      x <- [False, True]
      y <- [False, True]
      let inpMap =
            M.union (M.fromList [(IVX n, x), (IVY n, y)]) $
              M.fromList [(mk v, False) | mk <- [IVX, IVY], v <- [0 .. 44]]
          expected =
            S.fromList . catMaybes $
              [ n <$ guard (x /= y)
              , (n + 1) <$ guard (x && y)
              ]
          result = runRules swappedRules inpMap
      traceM $ show (result, expected)
      pure $ result == expected

--   --   where
--   --     gates' :: Map (GateName (n + 1)) (Gate (GateName (n + 1)))
--   --     gates' = M.fromList
--   --       [ (review _Swappable k', g')
--   --         | (preview _Swappable -> Just k, g) <- M.toList gates
--   --       , (k', g') <- case M.lookup k candidate of
--   --           Nothing -> pure (weakenSwappable k, undefined)
--   --       ]

-- findSwaps ::
--   forall n m.
--   (KnownNat n, KnownNat m) =>
--     Proxy n ->
--   Map (Swappable m) (Swappable m) ->
--   Map (GateName m) (Gate (GateName m)) ->
--   Maybe (Map (Swappable m) (Swappable m))
-- findSwaps _ swaps gates = case testEquality (SNat @n) (SNat @m) of
--     Just Refl -> Just swaps
--     Nothing
--       | validateSwaps swaps -> undefined
--         -- findSwaps @(n+1) swaps' gates
--       | otherwise -> undefined
--           -- goodSwap <- find validateSwaps do
--           --   candidate:rest <- tails allCandidates
--           --   guard $ candidate `M.notMember` swaps
--           --   _
--           -- findSwaps goodSwap gates
--       -- | validateSwaps swaps -> findSwaps
--     -- | M.empty gates' = undefined
--     -- | validateSwaps swaps = undefined
--     -- | otherwise = undefined
--   where
--     allCandidates = mapMaybe (preview _Swappable) $ M.keys gates
--     -- swaps' = weakenMap weakenSwappable swaps
--     -- gates' :: Map (GateName (n + 1)) (Gate (GateName (n + 1)))
--     -- gates' = strengthenMap strengthenGateName gates
--     validateSwaps :: Map (Swappable m) (Swappable m) ->  Bool
--     validateSwaps candidate = and do
--         x <- [False, True]
--         y <- [False, True]
--         let inpMap =
--               M.union (M.fromList [(IVX maxBound, x), (IVY maxBound, y)]) $
--                 M.fromList [(mk v, False) | mk <- [IVX, IVY], v <- finites]
--             -- applySwap = over _Swappable (\n -> maybe n _ $ M.lookup n candidate)
--             expected = S.fromList . catMaybes $
--               [ pred maxBound <$ guard (x /= y)
--               , maxBound <$ guard (x && y)
--               ]
--         pure $ runRules gates' inpMap == expected
--       where
--         gates' :: Map (GateName (n + 1)) (Gate (GateName (n + 1)))
--         gates' = M.fromList
--           [ (review _Swappable k', g')
--             | (preview _Swappable -> Just k, g) <- M.toList gates
--           , (k', g') <- case M.lookup k candidate of
--               Nothing -> pure (weakenSwappable k, undefined)
--           ]

--     applySwaps :: Map String String -> String -> String
--     applySwaps mp x = M.findWithDefault x x mp
-- solved = runRules gates' _
-- runRules ::
--   Map (GateName n) (Gate (GateName n)) ->
--   Map (InVar n) Bool ->
--   Set (Finite (n + 1))

-- findSwaps
--     :: Map String String
--     -> [(Gate (GateName 45), GateName 45)]
--     -> [String]
-- findSwaps = _

day24b :: [(Gate GateName, GateName)] :~> _ -- [Swappable]
day24b =
  MkSol
    { sParse = fmap snd . sParse day24a
    , -- , sShow = show
      sShow = intercalate "," . sort . map showSwappable
    , -- , sShow = intercalate "," . map showSwappable
      sSolve = fmap M.keys . listToMaybe . alignShells . M.fromList . map swap
      -- , sSolve = fmap M.keys . findSwaps 0 S.empty M.empty . M.fromList . map swap
      -- findSwaps @1 M.empty . M.fromList . map swap
      -- flip loopEither (0, M.empty) \(i, subs) ->
      --   case nameTree (M.fromList xs) subs (unrollAdderTree i) of
      --     Nothing -> Left $ M.keys subs
      --     Just subs' -> Right (i + 1, subs')
    }

-- showVarBit :: VarBit -> String
-- showVarBit VB{..} = printf (asString "%s%02d") vstr vbBit
--   where
--     vstr = asString case vbVar of
--       VX -> "x"
--       VY -> "y"
--       VZ -> "z"

-- type GateTree = Free Gate

-- halfAdder :: GateTree a -> GateTree a -> (GateTree a, GateTree a)
-- halfAdder x y = (wrap $ Gate OAnd x y, wrap $ Gate OXor x y)

-- fullAdder :: GateTree a -> GateTree a -> GateTree a -> (GateTree a, GateTree a)
-- fullAdder x y carry0 = (wrap $ Gate OOr carry1 carry2, o)
--   where
--     (carry1, z) = halfAdder x y
--     (carry2, o) = halfAdder z carry0

-- adderTree :: Int -> (GateTree VarBit, NonEmpty (GateTree VarBit))
-- adderTree n
--   | n == 0 = (:| []) `second` halfAdder (pure (VB VX 0)) (pure (VB VY 0))
--   | otherwise =
--       let (carryIn, rest) = adderTree (n - 1)
--           (carryOut, new) = fullAdder (pure (VB VX n)) (pure (VB VY n)) carryIn
--        in (carryOut, new `NE.cons` rest)

-- unrollGates ::
--   forall a. Ord a => GateTree a -> State (Int, Map (Gate (Either Int a)) Int) (Either Int a)
-- unrollGates = iterA go . fmap Right
--   where
--     go g0 = do
--       gate <- sequenceA g0
--       (currIx, currMp) <- get
--       case M.lookup gate currMp of
--         Nothing -> do
--           put (currIx + 1, M.insert gate currIx currMp)
--           pure $ Left currIx
--         Just i -> pure $ Left i

-- unrollAdderTree :: Int -> IntMap (Gate (Either Int VarBit))
-- unrollAdderTree n = IM.fromList $ swap <$> M.toList mp
--   where
--     (carry, adder) = adderTree n
--     full = carry `NE.cons` adder
--     (_, mp) = execState (traverse unrollGates full) (0, M.empty)

-- data NameState = NS
--   { nsRenames :: Map String String
--   , nsNames :: IntMap String
--   }
--   deriving stock (Generic, Show, Eq, Ord)
--   deriving anyclass (NFData)

-- nameGate ::
--   Map (Gate String) String ->
--   Int ->
--   Int ->
--   Gate (Either Int VarBit) ->
--   LogicT (StateT NameState Maybe) String
-- nameGate avail renameLimit ng g0 = do
--   NS{..} <- get
--   let gate = either (nsNames IM.!) showVarBit <$> g0
--   Just here <- pure $ applySwaps nsRenames <$> M.lookup gate avail
--   (here <$ (#nsNames %= IM.insert ng here))
--     `interleave` foldr
--       interleave
--       empty
--       [ there <$ put (NS renames (IM.insert ng there nsNames))
--       | here `M.notMember` nsRenames
--       , here `notElem` nsNames
--       , M.size nsRenames < renameLimit
--       , there <- toList avail
--       , here /= there
--       , there `M.notMember` nsRenames
--       , there `notElem` nsNames
--       , let renames = M.fromList [(here, there), (there, here)] <> nsRenames
--       ]
--   where
--     applySwaps :: Map String String -> String -> String
--     applySwaps mp x = M.findWithDefault x x mp

-- nameTree ::
--   Map (Gate String) String ->
--   Map String String ->
--   IntMap (Gate (Either Int VarBit)) ->
--   Maybe (Map String String)
-- nameTree avail renames0 =
--   fmap nsRenames
--     . flip execStateT s0
--     . observeT
--     . IM.traverseWithKey (nameGate avail (min 8 $ M.size renames0 + 2))
--   where
--     s0 = NS renames0 IM.empty

-- day24b :: [(Gate String, String)] :~> [String]
-- day24b =
--   MkSol
--     { sParse = fmap snd . sParse day24a
--     , sShow = intercalate ","
--     , sSolve = noFail \xs ->
--         flip loopEither (0, M.empty) \(i, subs) ->
--           case nameTree (M.fromList xs) subs (unrollAdderTree i) of
--             Nothing -> Left $ M.keys subs
--             Just subs' -> Right (i + 1, subs')
--     }
