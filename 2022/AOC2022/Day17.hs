-- |
-- Module      : AOC2022.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day17 (
  day17a,
  day17b,
)
where

import AOC.Common (findLoopBy, floodFill, skipConsecutiveBy)
import AOC.Common.Point (Point, cardinalNeighbs, shiftToZero')
import AOC.Solver ((:~>) (..))
import Control.Lens (view)
import Control.Monad ((<=<))
import Data.Bifunctor (second)
import Data.Finite (Finite, getFinite)
import Data.List (find)
import Data.Semigroup (Max (..))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector.Sized as V
import GHC.TypeNats (KnownNat)
import Linear (V2 (..), _y)

data Wind = L | R
  deriving stock (Show)

windDir :: Wind -> Point
windDir = \case
  L -> V2 (-1) 0
  R -> V2 1 0

parseWind :: Char -> Maybe Wind
parseWind '<' = Just L
parseWind '>' = Just R
parseWind _ = Nothing

data Block = Horiz | Plus | Corner | Vert | Box
  deriving stock (Eq, Ord, Show)

nextBlock :: Block -> Block
nextBlock = \case
  Horiz -> Plus
  Plus -> Corner
  Corner -> Vert
  Vert -> Box
  Box -> Horiz

-- | relative to bottom left corner of AABB
blockPoints :: Point -> Block -> Set Point
blockPoints p = \case
  Horiz -> S.fromList [p, p + V2 1 0, p + V2 2 0, p + V2 3 0]
  Plus -> S.fromList [p + V2 1 2, p + V2 0 1, p + V2 1 1, p + V2 2 1, p + V2 1 0]
  Corner -> S.fromList [p + V2 2 2, p + V2 2 1, p, p + V2 1 0, p + V2 2 0]
  Vert -> S.fromList [p, p + V2 0 1, p + V2 0 2, p + V2 0 3]
  Box -> S.fromList [p + V2 0 1, p + V2 1 1, p, p + V2 1 0]

-- | Whether or not a block would be in bounds if made at the given
-- point
inBounds :: Point -> Block -> Bool
inBounds p@(V2 x _) b =
  all (>= 0) p && case b of
    Horiz -> x < 4
    Plus -> x < 5
    Corner -> x < 5
    Vert -> x < 7
    Box -> x < 6

blowBlock :: Set Point -> Wind -> Point -> Block -> Point
blowBlock rocks w p b
  | canMove = p'
  | otherwise = p
  where
    p' = p + windDir w
    canMove = inBounds p' b && S.null (blockPoints p' b `S.intersection` rocks)

downBlock :: Set Point -> Point -> Block -> Either (Set Point) Point
downBlock rocks p b
  | canMove = Right p'
  | otherwise = Left (blockPoints p b `S.union` rocks)
  where
    p' = p + V2 0 (-1)
    canMove = inBounds p' b && S.null (blockPoints p' b `S.intersection` rocks)

boardTop :: Set Point -> Int
boardTop = (+ 1) . max 0 . getMax . foldMap (Max . view _y)

data RockState n = RS
  { rsWindIx :: !(Finite n)
  , rsBlockShape :: !Block
  , rsBlockPos :: !Point
  , rsBoard :: !(Set Point)
  , rsNumBlocks :: !Int
  }
  deriving stock (Show)

stepState :: KnownNat n => V.Vector n Wind -> RockState n -> RockState n
stepState ws RS{..} = case downBlock rsBoard blownPos rsBlockShape of
  Left rsBoard' ->
    RS rsWindIx' (nextBlock rsBlockShape) (V2 2 (boardTop rsBoard' + 3)) rsBoard' (rsNumBlocks + 1)
  Right rsBlockPos' ->
    RS rsWindIx' rsBlockShape rsBlockPos' rsBoard rsNumBlocks
  where
    w = ws `V.index` rsWindIx
    rsWindIx' = rsWindIx + 1
    blownPos = blowBlock rsBoard w rsBlockPos rsBlockShape

day17a :: [Wind] :~> Set Point
day17a =
  MkSol
    { sParse = traverse parseWind
    , sShow = show . boardTop
    , sSolve = \ws -> V.withSizedList ws \wsVec ->
        fmap rsBoard
          . find (\rs -> rsNumBlocks rs >= 2022)
          . iterate (stepState wsVec)
          $ RS 0 Horiz (V2 2 3) S.empty 0
    }

topShape :: Set Point -> Set Point
topShape board = shiftToZero' $ floodFill expand (S.singleton (V2 0 maxY))
  where
    maxY = boardTop board
    expand pt =
      S.fromList
        [ r
        | r@(V2 x y) <- cardinalNeighbs pt
        , x >= 0 && x < 7
        , y >= 0 && y <= maxY
        , r `S.notMember` board
        ]

day17b :: _ :~> _
day17b =
  MkSol
    { sParse = traverse parseWind
    , sShow = show . boardTop
    , sSolve = \ws -> V.withSizedList ws \wsVec ->
        fmap rsBoard
          . find (\rs -> rsNumBlocks rs >= tfinal)
          <=< fmap (iterate (stepState wsVec) . extrapolate . fmap (second fst))
            . findLoopBy snd
            . map (\rs -> (rs, (getFinite (rsWindIx rs), rsBlockShape rs, topShape (rsBoard rs))))
            . skipConsecutiveBy rsNumBlocks
            . iterate (stepState wsVec)
          $ RS 0 Horiz (V2 2 3) S.empty 0
    }
  where
    tfinal :: Int
    tfinal = 1000000000000
    extrapolate (V2 (i0, s0) (i1, s1)) =
      s1
        { rsBoard = S.map (+ V2 0 (dtop * n)) (rsBoard s0)
        , rsBlockPos = rsBlockPos s0 + V2 0 (dtop * n)
        , rsNumBlocks = rsNumBlocks s0 + n * (i1 - i0)
        }
      where
        totalStepsLeft = tfinal - i0
        n = totalStepsLeft `div` (i1 - i0)
        top0 = boardTop (rsBoard s0)
        top1 = boardTop (rsBoard s1)
        dtop = top1 - top0
