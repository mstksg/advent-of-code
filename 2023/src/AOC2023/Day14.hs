{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!
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
module AOC2023.Day14 (
  day14a,
  day14b,
)
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import Data.Group
import qualified Data.IntMap.NonEmpty as NEIM
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map as M
import qualified Data.OrdPSQ as PSQ
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

data PillarData = PD
  { pdNorth :: Pillars
  , pdEast :: Pillars
  , pdSouth :: Pillars
  , pdWest :: Pillars
  }

data Pillars = Pillars
  { pCols :: IntMap IntSet
  , pMin :: Int
  , pMax :: Int
  }

splitPoints :: Map Point Bool -> (PillarData, [Point])
splitPoints mp = (PD (go North) (go East) (go South) (go West), rocks)
  where
    go dir =
      Pillars
        (IM.keysSet . NEIM.filter id <$> cols)
        (minimum $ fst . NEIM.findMin <$> IM.elems cols)
        (maximum $ fst . NEIM.findMax <$> IM.elems cols)
      where
        cols :: IntMap (NEIntMap Bool)
        cols =
          IM.fromListWith
            (<>)
            [ (x, NEIM.singleton y v)
            | (V2 x y, v) <- first (rotPoint dir) <$> M.toList mp
            ]
    rocks = M.keys $ M.filter not mp

shiftDir :: PillarData -> Dir -> [Point] -> [Point]
shiftDir PD{..} dir rs =
  [ rotPoint (invert dir) $ V2 x y
  | (x, ys) <- IM.toList fallens
  , y <- ys
  ]
  where
    Pillars{..} = case dir of
      North -> pdNorth
      East -> pdEast
      South -> pdSouth
      West -> pdWest
    cols :: IntMap IntSet
    cols =
      IM.fromListWith
        (<>)
        [ (x, IS.singleton y)
        | V2 x y <- rotPoint dir <$> rs
        ]
    fallens :: IntMap [Int]
    fallens = IM.intersectionWith go pCols cols
      where
        go pCol col =
          [ j
          | (i, n) <- IM.toList fallen
          , j <- [i + 1 .. i + n]
          ]
          where
            fallen :: IntMap Int
            fallen = IS.foldl' mkPiles IM.empty col
            mkPiles !curr i = IM.insertWith (+) (fromMaybe (pMin - 1) $ IS.lookupLT i pCol) 1 curr

score :: Int -> [Point] -> Int
score maxRow pts = sum $ pts <&> \(V2 _ y) -> maxRow - y + 1

shiftCycle :: PillarData -> [Point] -> [Point]
shiftCycle pd =
  shiftDir pd East
    . shiftDir pd South
    . shiftDir pd West
    . shiftDir pd North

day14a :: _ :~> _
day14a =
  MkSol
    { sParse = noFail $
        parseAsciiMap $ \case
          'O' -> Just False
          '#' -> Just True
          _ -> Nothing
    , sShow = show
    , sSolve = noFail $ \mp ->
        let (ps, rs) = splitPoints mp
         in score (pMax (pdNorth ps)) $ shiftDir ps North rs
    }

day14b :: _ :~> _
day14b =
  MkSol
    { sParse = sParse day14a
    , sShow = show
    , sSolve = \mp -> do
        let (ps, rs) = splitPoints mp
            shifts = iterate (shiftCycle ps) rs
        V2 i j <- fmap fst <$> findLoopBy id shifts
        let leftover = (1000000000 - i) `mod` (j - i)
        pure $ score (pMax (pdNorth ps)) $ shifts !!! (i + leftover)
    }
