{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2025.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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
module AOC2025.Day07 (
  day07a,
  day07b,
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

day07a :: _ :~> _
day07a =
  MkSol
    { sParse =
        noFail $
          parseAsciiMap $ \case 'S' -> Just True
                                '^' -> Just False
                                _ -> Nothing
    , sShow = show
    , sSolve =
        noFail $ \mp ->
              let startPos = S.fromList [ p | (p, True) <- M.toList mp ]
-- boundingBox' :: (Foldable f, Applicative g, Ord a) => f (g a) -> Maybe (V2 (g a))
-- boundingBox' = fmap boundingBox . NE.nonEmpty . toList
                  Just bb@(V2 _ (V2 _ maxY)) = boundingBox' (M.keys mp)
                  filled = flip floodFill startPos $ \x -> S.filter (inBoundingBox bb)
                              case M.lookup (x + V2 0 1) mp of
                                      Nothing -> S.singleton $ x + V2 0 1
                                      Just _ -> S.fromList [x + V2 1 1, x + V2 (-1) 1]
               in length [ () | (p, False) <- M.toList mp, (p - V2 0 1) `S.member` filled]
               -- length [ () | V2 _ y <- S.toList filled, y == maxY]
    }

-- -- | Flood fill from a starting set
-- floodFill ::
--   Ord a =>
--   -- | Expansion (be sure to limit allowed points)
--   (a -> Set a) ->
--   -- | Start points
--   Set a ->
--   -- | Flood filled
--   Set a
-- floodFill f = snd . floodFillCount f

day07b :: _ :~> _
day07b =
  MkSol
    { sParse = sParse day07a
    , sShow = show
    , sSolve =
        noFail $ \mp ->
              -- let startPos = head [ p | (p, True) <- M.toList mp ]
              --     Just bb@(V2 _ (V2 _ maxY)) = boundingBox' (M.keys mp)
              --     go = do 
              --       StateT $ \x ->
              --                 case M.lookup (x + V2 0 1) mp of
              --                   Nothing -> [((),x + V2 0 1)]
              --                   Just _ -> map ((),) [x + V2 1 1, x + V2 (-1) 1]
              --       x' <- get
              --       if inBoundingBox bb x'
              --          then go
              --          else pure ()
              --  in length $ execStateT go startPos
              --
              let startPos = head [ p | (p, True) <- M.toList mp ]
                  Just bb@(V2 _ (V2 _ maxY)) = boundingBox' (M.keys mp)
                  pathsFrom :: Map Point Int
                  pathsFrom = flip M.fromSet (fillBoundingBox bb) $ \p@(V2 x y) ->
                    if y == maxY
                       then 1
                       else  case M.lookup (p + V2 0 1) mp of
                               Just False -> M.findWithDefault 0 (p + V2 1 1) pathsFrom + M.findWithDefault 0 (p + V2 (-1) 1) pathsFrom
                               Nothing -> pathsFrom M.! (p + V2 0 1)
                               Just True -> pathsFrom M.! (p + V2 0 1)
               in pathsFrom M.! startPos + 2
    }
