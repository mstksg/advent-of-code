{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day24
where

-- (
-- day24a,
-- day24b,
-- adderTree,
-- gateTreeTree,
-- )

import AOC.Prelude
import Control.Monad.Free
import Control.Monad.Logic
import Control.Monad.Trans.Writer.CPS
import Data.Functor.Classes
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
import qualified Data.Tree as Tree
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import GHC.Generics
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

-- x00: 1
-- x01: 1
-- x02: 0
-- x03: 0
-- x04: 0
-- x05: 1
-- x06: 0
-- x07: 1
-- x08: 1
-- x09: 0

data Op = OAnd | OOr | OXor
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data Gate a = Gate {gOp :: Op, gX :: a, gY :: a}
  deriving stock (Show, Generic, Functor, Traversable, Foldable)
  deriving anyclass (NFData)

instance Show1 Gate where
  liftShowsPrec sp _ d (Gate o x y) = showsBinaryWith sp sp (show o) d x y

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

parseGate :: CharParser (Gate String)
parseGate = do
  gX <- pAlphaNumWord
  gOp <-
    P.choice
      [ OAnd <$ "AND"
      , OOr <$ "OR"
      , OXor <$ "XOR"
      ]
  gY <- pAlphaNumWord
  pure Gate{..}

parseInitial :: CharParser (String, Bool)
parseInitial = (,) <$> many P.alphaNumChar <* ": " <*> tokenAssoc [('0', False), ('1', True)]

applyOp :: Op -> Bool -> Bool -> Bool
applyOp = \case
  OAnd -> (&&)
  OOr -> (||)
  OXor -> (/=)

applyGate :: Gate Bool -> Bool
applyGate Gate{..} = applyOp gOp gX gY

day24a :: ([(String, Bool)], [(Gate String, String)]) :~> _
day24a =
  MkSol
    { sParse = parseMaybe' do
        cs <- P.many $ parseInitial <* P.newline
        P.newline
        os <- sepByLines $ (,) <$> parseGate <* "-> " <*> P.many P.alphaNumChar
        pure (cs, os)
    , sShow = show
    , sSolve =
        noFail \(st, xs) ->
          let rules = M.fromList $ swap <$> xs
              res = M.fromList st <> (applyGate . fmap (res M.!) <$> rules)
           in parseBinary . reverse . toList $ M.filterWithKey (\k _ -> "z" `isPrefixOf` k) res
    }

data Var = VX | VY | VZ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data VarBit = VB {vbVar :: Var, vbBit :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

showVarBit :: VarBit -> String
showVarBit VB{..} = printf (asString "%s%02d") vstr vbBit
  where
    vstr = asString case vbVar of
      VX -> "x"
      VY -> "y"
      VZ -> "z"

type GateTree = Free Gate

gateTreeTree :: GateTree a -> Tree.Tree (Either Op a)
gateTreeTree = iter (\Gate{..} -> Tree.Node (Left gOp) [gX, gY]) . fmap (flip Tree.Node [] . Right)

halfAdder :: GateTree a -> GateTree a -> (GateTree a, GateTree a)
halfAdder x y = (wrap $ Gate OAnd x y, wrap $ Gate OXor x y)

fullAdder :: GateTree a -> GateTree a -> GateTree a -> (GateTree a, GateTree a)
fullAdder x y carry0 = (wrap $ Gate OOr carry1 carry2, o)
  where
    (carry1, z) = halfAdder x y
    (carry2, o) = halfAdder z carry0

adderTree :: Int -> (GateTree VarBit, NonEmpty (GateTree VarBit))
adderTree n
  | n == 0 = (:| []) `second` halfAdder (pure (VB VX 0)) (pure (VB VY 0))
  | otherwise =
      let (carryIn, rest) = adderTree (n - 1)
          (carryOut, new) = fullAdder (pure (VB VX n)) (pure (VB VY n)) carryIn
       in (carryOut, new `NE.cons` rest)

unrollGates ::
  forall a. Ord a => GateTree a -> State (Int, Map (Gate (Either Int a)) Int) (Either Int a)
unrollGates = iterA go . fmap Right
  where
    go g0 = do
      gate <- sequenceA g0
      (currIx, currMp) <- get
      case M.lookup gate currMp of
        Nothing -> do
          put (currIx + 1, M.insert gate currIx currMp)
          pure $ Left currIx
        Just i -> pure $ Left i

unrollAdderTree :: Int -> (NonEmpty (Either Int VarBit), IntMap (Gate (Either Int VarBit)))
unrollAdderTree n = (out, IM.fromList $ swap <$> M.toList mp)
  where
    (carry, adder) = adderTree n
    full = carry `NE.cons` adder
    (out, (_, mp)) = flip runState (0, M.empty) $ traverse unrollGates full

-- renameTree :: Map String (Gate String) -> IntMap (Gate (Either Int VarBit)) -> Map String (Gate (Either String VarBit))
-- renameTree ref = _

data NameState = NS
  { nsRenames :: Map String String
  , nsNames :: IntMap String
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData)

nameGate ::
  Map (Gate String) String ->
  Int ->
  Gate (Either Int VarBit) ->
  LogicT (StateT NameState Maybe) String
nameGate avail ng Gate{..} = case (gX, gY) of
  (Left i, Left j) -> do
    NS{..} <- get
    let nX = nsNames IM.! i
        nY = nsNames IM.! j
    let gate = Gate gOp nX nY
    case applySwaps nsRenames <$> M.lookup gate avail of
      Nothing -> do
        traceM $ "backtrack on " <> show ng <> ": " <> show gate <> " " <> show (M.keys nsRenames)
        empty -- reject this branch, needs a rename
      Just here ->
        (do traceM $ "try on " <> show ng <> ": " <> show gate <> " -> " <> here <> " " <> show (M.keys nsRenames)
            here <$ (#nsNames %= IM.insert ng here)
        )
          `interleave` foldr
            interleave
            empty
            [ there <$ put (NS renames (IM.insert ng there nsNames))
            -- there <$ (#nsNames %= IM.insert ng there)
            | here `M.notMember` nsRenames
            , here `notElem` nsNames
            , M.size nsRenames < 8
            , there <- toList avail
            , here /= there
            , there `M.notMember` nsRenames
            , there `notElem` nsNames
            , let renames = M.fromList [(here, there), (there, here)] <> nsRenames
            ]
  (Right x, Right y) -> do
    NS{..} <- get
    let gate = showVarBit <$> Gate gOp x y
    case applySwaps nsRenames <$> M.lookup gate avail of
      Nothing -> error $ show gate
      Just here ->
        (do traceM $ "try on " <> show ng <> ": " <> show gate <> " -> " <> here <> " " <> show (M.keys nsRenames)
            here <$ (#nsNames %= IM.insert ng here)
        )
        -- (here <$ (#nsNames %= IM.insert ng here))
          `interleave` foldr
            interleave
            empty
            [ there <$ put (NS renames (IM.insert ng there nsNames))
            -- there <$ (#nsNames %= IM.insert ng there)
            | here `M.notMember` nsRenames
            , here `notElem` nsNames
            , M.size nsRenames < 8
            , there <- toList avail
            , here /= there
            , there `M.notMember` nsRenames
            , there `notElem` nsNames
            , let renames = M.fromList [(here, there), (there, here)] <> nsRenames
            ]
  _ -> error "huh"
  where
    applySwaps :: Map String String -> String -> String
    applySwaps mp x = M.findWithDefault x x mp

nameTree :: Map (Gate String) String -> IntMap (Gate (Either Int VarBit)) -> Maybe (IntMap String, Map String String)
nameTree avail = fmap (second nsRenames) . flip runStateT s0 . observeT . IM.traverseWithKey (nameGate avail)
  where
    s0 = NS
      -- (M.fromList [("bjm","z07"),("hsw","z13"),("z07","bjm"),("z13","hsw")])
      -- (M.fromList [("bjm","z07"),("hsw","z13"),("skf","z18"),("z07","bjm"),("z13","hsw"),("z18","skf")])
      (M.fromList [("bjm","z07"),("hsw","z13"),("nvr","wkr"),("skf","z18"),("wkr","nvr"),("z07","bjm"),("z13","hsw"),("z18","skf")])
      -- M.empty
      IM.empty

-- j <- case gX of
--   Left x -> do
--     nameGate avail x _
--   Right v -> pure (Right v)
-- undefined

-- go :: Gate (State (IntMap (Gate (Either Int a))) (Either Int a)) -> State (IntMap (Gate (Either Int a))) (Either Int a)
-- go Gate{..} = do
--   x <- gX
--   y <- gY
--   modify \im ->
--     let i = maybe 0 (succ . fst) $ IM.lookupMax im
--      in IM.insert i (Gate gOp x y) im
--   pure undefined

-- where
--   go :: Gate (Const (Set (Gate (Either Int a))) a) -> Const (Set (Gate (Either Int a))) a
--   go Gate{..} = Const $ _ gOp (getConst gX) (getConst gY)
-- go :: Gate (Const (Set (Gate ())) a) -> Const (Set (Gate ())) a
-- go Gate{..} = _ gOp (getConst gX) (getConst gY)

data SearchState = SS
  { ssAvailable :: !(Map (Gate String) String)
  , ssSwaps :: !(Map String String)
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData)

-- this seems to work, we just need to record failures and swaps
--
-- okay so instead of recording failures and swaps, let's perform the swaps.
-- since we don't need to backtrqack swaps
labelTree :: GateTree String -> LogicT (StateT SearchState Maybe) String
labelTree = iterM \Gate{..} ->
  gX >>= \x ->
    gY >>= \y ->
      let gate = Gate gOp x y
       in get >>= \SS{..} ->
            case M.updateLookupWithKey (\_ _ -> Nothing) gate ssAvailable of
              (Nothing, _) -> empty -- bad label, backtrack?
              (Just l, mp') ->
                (l <$ (#ssAvailable .= mp'))
                  <|> foldr
                    interleave
                    empty
                    [ l' <$ put (SS{ssAvailable = mp'', ssSwaps = M.insert l l' ssSwaps})
                    | (g', l') <- M.toList mp'
                    , l' /= l
                    , let mp'' = M.insert g' l mp'
                    ]

labelTree' ::
  Map (Gate String) String -> GateTree String -> LogicT (WriterT (Map String String) Maybe) String
labelTree' mp = iterM \Gate{..} ->
  gX >>= \x ->
    gY >>= \y ->
      case M.lookup (Gate gOp x y) mp of
        Nothing -> empty -- bad label, backtrack?
        Just l ->
          do
            pure l
            `interleave` foldr
              interleave
              empty
              [ l' <$ lift (tell (M.singleton l l'))
              | l' <- toList (S.delete l labels)
              ]
  where
    labels = S.fromList $ toList mp

-- hm ok maybe do not need to delete because then yeah
labelAdder :: Int -> LogicT (StateT SearchState Maybe) (NonEmpty String)
labelAdder n = traverse labelTree (NE.reverse full)
  where
    (carry, adder) = adderTree n
    full = fmap showVarBit <$> (carry `NE.cons` adder)

-- hm ok maybe do not need to delete because then yeah
labelAdder' ::
  Int -> Map (Gate String) String -> LogicT (WriterT (Map String String) Maybe) (NonEmpty String)
labelAdder' n mp = traverse (labelTree' mp) (NE.reverse full)
  where
    (carry, adder) = adderTree n
    full = fmap showVarBit <$> (carry `NE.cons` adder)

-- adderTree :: Int -> (GateTree VarBit, NonEmpty (GateTree VarBit))
-- adderTree n
--   | n == 0 = (:| []) `second` halfAdder (pure (VB VX 0)) (pure (VB VY 0))
--   | otherwise =
--       let (carryIn, rest) = adderTree (n - 1)
--           (carryOut, new) = fullAdder (pure (VB VX n)) (pure (VB VY n)) carryIn
--        in (carryOut, new `NE.cons` rest)

-- pickHere
-- (l <$ modify (M.delete l))
-- `interleave` asum
--   [ _
--     | l <- toList mp
--     , let mp' = _
--     , l /= l'
--   ]

-- [ tryPop
-- -- l' <$ lift (tell (M.singleton l l'))
--             | l' <- toList (S.delete l labels)
--          ]
-- where
--   labels = S.fromList $ toList mp

-- case (x, y) of
--   (Left a, Left b) -> _
-- _ x y

-- _ gOp gX gY
-- traverse _

-- strategy: traverse until you find a mistake
--
-- or maybe build bottom-up with the pieces available?
--
-- yeah build bottom-up sounds like a good idea. and then we can diff the two
-- trees via zip
day24b :: _ :~> _
day24b =
  MkSol
    { sParse = fmap snd . sParse day24a
    , -- , sShow = ('\n' :)
      sShow = intercalate ","
    , -- , sShow = show
      sSolve = \xs ->
        let rules = M.fromList (first normalizeGate <$> xs)
            -- swaps :: [V2 String]
            -- swaps = [V2 "z07" "bjm", V2 "z13" "hsw"]
            -- applySwap (V2 x y) mp = M.fromList [(kx, y), (ky, x)] <> mp
            --   where
            --     Just kx = findKeyFor x mp
            --     Just ky = findKeyFor y mp
            -- rules' = foldl' (flip applySwap) rules swaps
            (ual, uat) = unrollAdderTree 44
        in M.keys . snd <$> nameTree rules uat
         -- in unlines
         --      [ show ual
         --      , show $ nameTree rules uat
         --      , show uat
         --      ]
        -- in (ual, uat, nameTree rules uat)

        -- swaps :: [V2 String]
        -- swaps = [V2 "z07" "bcv", V2 "z13" "bcv"]
        -- applySwap (V2 x y) mp = M.fromList [(kx, x), (ky, y)] <> mp
        --   where
        --     Just kx = findKeyFor x mp
        --     Just ky = findKeyFor y mp
        -- rules' = foldl' (flip applySwap) rules swaps
        -- s0 = SS rules M.empty
        -- in runWriterT . observeT $ labelAdder' 17 rules
        -- in flip runStateT s0 . observeT $ labelAdder 5
        -- in flip runStateT s0 . observeT $ labelTree $ fmap showVarBit . fst $ adderTree 18
        -- listToMaybe . runWriterT $ labelTree rules $ fmap showVarBit . fst $ adderTree 18
        -- show $ M.fromList (first normalizeGate <$> xs)
        -- let rules = M.fromList $ swap <$> xs
        --     res :: Map String (GateTree String)
        --     res = wrap . fmap (\k -> M.findWithDefault (pure k) k res) <$> rules
        --     res' = M.filterWithKey (\k _ -> k == "z02") res
        -- in Tree.drawForest $ M.toList res' <&> \(k, v) -> Tree.Node k [either show id <$> gateTreeTree v]

        -- res =
        --   rules <&> \(V3 a b c) ->
        --     -- let y = M.findWithDefault (res M.! a) a starts
        --     --     z = M.findWithDefault (res M.! c) c starts
        --     let y = maybe (Tree.Node (Right a) []) id $ M.lookup a res
        --         -- M.findWithDefault (res M.! a) a starts
        --         z = maybe (Tree.Node (Right c) []) id $ M.lookup c res
        --      in -- z = M.findWithDefault (res M.! c) c starts
        --         Tree.Node (Left b) [y, z]
        -- in -- case b of
        -- --         "AND" -> y && z
        -- --         "OR" -> y || z
        -- --         "XOR" -> y /= z
        -- -- res = rules <&> \(V3 a b c) ->
        -- --   let y = maybe (Left a) Right $ M.lookup a res
        -- --       z = maybe (Left c) Right $ M.lookup c res
        -- --   in  (b, y, z)
        -- -- case b of
        -- --         "AND" -> y && z
        -- --         "OR" -> y || z
        -- --         "XOR" -> y /= z
        -- Tree.drawForest $
        --   (\(k, v) -> Tree.Node k [either id id <$> v])
        --     <$> M.toList (M.filterWithKey (\k _ -> "z" `isPrefixOf` k) res)
    }
