-- |
-- Module      : AOC2020.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
module AOC2020.Day20 (
  day20a,
  day20b,
)
where

import AOC.Common (mapMaybeSet)
import AOC.Common.Point (
  D8 (..),
  Dir (..),
  FinPoint,
  Point,
  allD8,
  allDir,
  boundingBox,
  boundingBox',
  orientFin,
  orientPoint,
  parseAsciiSet,
  rotPoint,
  shiftToZero,
 )
import AOC.Solver ((:~>) (..))
import Control.Lens hiding (uncons)
import Control.Monad ((<=<))
import Data.Bit (Bit (..))
import Data.Char (isDigit)
import Data.Distributive (distribute)
import Data.Finitary (fromFinite, toFinite)
import Data.Finite (packFinite, strengthen, unshift)
import Data.Foldable (find, toList)
import Data.Group (invert)
import Data.IntMap (IntMap)
import Data.IntMap.NonEmpty (NEIntMap)
import qualified Data.IntMap.NonEmpty as NEIM
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Ix (range)
import Data.List (foldl', uncons)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEM
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Vector.Sized as V
import qualified Data.Vector.Unboxed.Sized as VU
import Linear (V2 (..))
import Text.Read (readMaybe)

type Edge = VU.Vector 10 Bit

type Core = Set (FinPoint 8)

-- | Convert a set of points into all the orientations of tiles it could
-- be, indexed by the north edge of that orientation.
toTiles :: NESet (FinPoint 10) -> ((Core, V.Vector 8 Edge), NEMap Edge D8)
toTiles ps = ((core, edges), edgeMap)
  where
    core = S.fromDistinctAscList . mapMaybe (traverse (strengthen <=< unshift)) . toList $ ps
    edges = V.generate $ \i ->
      let ps' = orientFin (invert (fromFinite i)) `NES.map` ps
       in VU.generate $ \j -> Bit $ V2 j 0 `NES.member` ps'
    edgeMap =
      NEM.fromList $
        allD8 <&> \o -> (edges `V.index` toFinite o, o)

type Placement = (Int, D8)

assembleMap ::
  NEIntMap (V.Vector 8 Edge) ->
  NEIntMap (NEMap Edge Placement) ->
  Map Point Placement
assembleMap tileMap tiles0 =
  go
    (toQueue 0 mempty t0id allDir)
    (IM.keysSet tiles1)
    (M.singleton 0 (t0id, mempty))
  where
    ((_, t0Map), tiles1) = NEIM.deleteFindMin tiles0
    ((_, (t0id, _)), _) = NEM.deleteFindMin t0Map
    tileCache :: NEMap Edge [Placement]
    tileCache =
      NEM.fromListWith
        (++)
        [ (edge, [placement])
        | (_, tileEdges) <- NEIM.toList tiles0
        , (edge, placement) <- NEM.toList tileEdges
        ]
    go ::
      Map Edge (Point, Dir) ->
      -- \^ queue: edge -> place, orientation
      IntSet ->
      -- \^ leftover points
      Map Point Placement ->
      -- \^ current map
      Map Point Placement
    -- \^ sweet tail rescursion
    go !queue !tiles !mp = case M.minViewWithKey queue of
      Nothing -> mp
      Just ((edge, (pos, d)), queue') ->
        case find ((`IS.member` tiles) . fst) (tileCache NEM.! edge) of
          Nothing -> go queue' tiles mp
          Just (tileId, o) ->
            let o' = o <> D8 (d <> South) True
                newQueue =
                  toQueue
                    pos
                    o'
                    tileId
                    (NE.filter (/= d <> South) allDir)
             in go
                  (newQueue <> queue)
                  (IS.delete tileId tiles)
                  (M.insert pos (tileId, invert o') mp)
    -- \| For a given image, add the given edges into the queue
    toQueue ::
      (Foldable f) =>
      Point ->
      -- \^ location of corner
      D8 ->
      -- \^ orientation to insert
      Int ->
      -- \^ tile id
      f Dir ->
      -- \^ edges to insert
      Map Edge (Point, Dir)
    toQueue p0 o tileId ds =
      M.fromList $
        toList ds <&> \d ->
          ( (tileMap NEIM.! tileId)
              `V.index` toFinite (o <> D8 d False)
          , (p0 + rotPoint d (V2 0 (-1)), d)
          )

solve ::
  NEIntMap (NESet (FinPoint 10)) ->
  (Map Point Placement, Set Point)
solve ts = (mp, blitted)
  where
    info = toTiles <$> ts
    edgeMap = flip NEIM.mapWithKey info \i (_, e) -> (i,) <$> e
    edges = snd . fst <$> info
    mp = assembleMap edges edgeMap
    blitted = flip M.foldMapWithKey mp $ \p (tileId, o) ->
      let core = fst . fst $ info NEIM.! tileId
       in S.map ((+ (p * 8)) . fmap fromIntegral . orientFin o) core

day20a :: IntMap (NESet (FinPoint 10)) :~> Int
day20a =
  MkSol
    { sParse = parseTiles
    , sShow = show
    , sSolve = \ts -> do
        (mp, _) <- solve <$> NEIM.nonEmptyMap ts
        bb <- distribute <$> boundingBox' (M.keys mp)
        pure $
          product
            [ fst $ mp M.! p
            | p <- traverse toList bb
            ]
    }

day20b :: IntMap (NESet (FinPoint 10)) :~> Int
day20b =
  MkSol
    { sParse = parseTiles
    , sShow = show
    , sSolve = \ts -> do
        (_, NES.IsNonEmpty blitted) <- solve <$> NEIM.nonEmptyMap ts
        listToMaybe
          [ res
          | drgn <- toList dragons
          , let res = S.size $ pokePattern (NES.toSet drgn) blitted
          , res /= NES.size blitted
          ]
    }

pokePattern ::
  Set Point ->
  NESet Point ->
  Set Point
pokePattern pat ps0 = foldl' go (NES.toSet ps0) (range (mn, mx))
  where
    V2 mn mx = boundingBox ps0
    go ps d
      | pat' `S.isSubsetOf` ps = ps S.\\ pat'
      | otherwise = ps
      where
        pat' = S.mapMonotonic (+ d) pat

dragons :: NonEmpty (NESet Point)
dragons = allD8 <&> \o -> shiftToZero $ NES.map (orientPoint o) dragon

dragon :: NESet Point
dragon =
  fromJust . NES.nonEmptySet . parseAsciiSet (== '#') $
    unlines
      [ "                  # "
      , "#    ##    ##    ###"
      , " #  #  #  #  #  #   "
      ]

parseTiles :: String -> Maybe (IntMap (NESet (FinPoint 10)))
parseTiles =
  fmap IM.fromList
    . traverse (uncurry go <=< uncons . lines)
    . splitOn "\n\n"
  where
    go tname tiles =
      (,)
        <$> readMaybe (filter isDigit tname)
        <*> NES.nonEmptySet (mapMaybeSet (traverse (packFinite . fromIntegral)) tileset)
      where
        tileset = parseAsciiSet (== '#') (unlines tiles)
