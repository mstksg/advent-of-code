{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day15 (
  day15a,
  day15b,
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

pdir = \case
  '^' -> Just South
  '>' -> Just East
  'v' -> Just North
  '<' -> Just West
  _ -> Nothing

day15a :: _ :~> _
day15a =
  MkSol
    { sParse = \xs -> case splitOn "\n\n" xs of
        [x, d] ->
          (,)
            <$> Just
              ( parseAsciiMap
                  ( \case
                      'O' -> Just (Just True)
                      '#' -> Just (Just False)
                      '@' -> Just Nothing
                      _ -> Nothing
                  )
                  x
              )
            <*> traverse pdir (filter (/= '\n') d)
    , -- noFail $
      --   lines
      sShow = show
    , sSolve =
        noFail \(mp, path) ->
          let walls = M.keysSet $ M.filter (== Just False) mp
              crates = M.keysSet $ M.filter (== Just True) mp
              Just person = fmap fst . find ((== Nothing) . snd) $ M.toList mp
           in sum . map score . toList . snd $ foldl' (step walls) (person, crates) path
    }

score :: Num a => V2 a -> a
score (V2 x y) = 100 * y + x

step :: Set Point -> (Point, Set (V2 Int)) -> Dir -> (Point, Set (V2 Int))
step obs (person, crates) d
  | person' `S.member` obs = (person, crates)
  | person' `S.member` crates =
      maybe (person, crates) ((person',) . S.delete person') $ tryMove person' crates
  | otherwise = (person', crates)
  where
    person' = person + dirPoint d
    tryMove p crates'
      | p' `S.member` obs = Nothing
      | p' `S.member` crates' = S.insert p' <$> tryMove p' crates'
      | otherwise = Just $ S.insert p' crates'
      where
        p' = p + dirPoint d

day15b :: _ :~> _
day15b =
  MkSol
    { sParse = sParse day15a
    , sShow = show
    -- , sShow = \(ps, obs) -> unlines . ("":) $ map (disp obs) ps
    , sSolve =
        noFail \(mp, path) ->
          let walls = M.keysSet . widen $ M.keysSet $ M.filter (== Just False) mp
              crates = widen $ M.keysSet $ M.filter (== Just True) mp
              Just person = fmap ((* V2 2 1) . fst) . find ((== Nothing) . snd) $ M.toList mp
           -- in (take 10 $ scanl' (step2 walls) (person, crates) path, walls)
           in sum . map score . M.keys . M.filter not . snd $ foldl' (step2 walls) (person, crates) path
    }
  where
    -- false: left, true: right
    widen :: Set Point -> Map Point Bool
    widen mp =
      M.fromList
        [ (p', l)
        | p <- S.toList mp
        , (l, p') <- [(False, p * V2 2 1), (True, p * V2 2 1 + V2 1 0)]
        ]
    disp obs (p, crates) = displayAsciiMap '.' $
        M.unionWith (\_ _ -> '!')
      (M.insertWith (\_ _ -> '!') p '@' ((\case False -> '['; True -> ']') <$> crates))
          (M.fromSet (const '#') obs)

step2 :: Set Point -> (V2 Int, Map (V2 Int) Bool) -> Dir -> (V2 Int, Map (V2 Int) Bool)
step2 obs (person, crates) d
    | person' `S.member` obs = (person, crates)
    | Just lr <- M.lookup person' crates = maybe (person, crates) (person',) $ tryMove lr
    | otherwise = (person', crates)
  where
    person' = person + dirPoint d
    tryMove moved
      | d `elem` [East, West] = tryMoveHoriz person' crates moved
      | otherwise = do 
          crates' <- tryMoveVert person' crates moved
          tryMoveVert q crates' (not moved)
      where
        q | moved = person' - V2 1 0
          | otherwise = person' + V2 1 0
    tryMoveHoriz p crates' moved
      | p' `S.member` obs = Nothing
      | otherwise = case M.lookup p' crates' of
          Just lr -> M.delete p . M.insert p' moved <$> tryMoveHoriz p' crates' lr
          Nothing -> Just $ M.delete p . M.insert p' moved $ crates'
      where
        p' = p + dirPoint d
    tryMoveVert p crates' moved
        | p' `S.member` obs = Nothing
        | Just lr <- M.lookup p' crates' = do
            let q'
                  | lr = p' - V2 1 0
                  | otherwise = p' + V2 1 0
            -- let p2 | lr = p - V2 1 0
            --        | otherwise = p + V2 1 0
            --     p2' | lr = p' - V2 1 0
            --         | otherwise = p' + V2 1 0
            crates0 <- tryMoveVert p' crates' lr
            crates1 <- tryMoveVert q' crates0 (not lr)
            -- crates1 <- tryMoveVert p2' crates0 (not lr)
            pure $ M.delete p . M.insert p' moved $ crates1
        | otherwise = do Just $ M.delete p $ M.insert p' moved crates'
              -- let p2 | v = p - V2 1 0
              --        | otherwise = p + V2 1 0
              --     p2' | v = p' - V2 1 0
              --         | otherwise = p' + V2 1 0
              -- in M.delete p2 . M.delete p $ M.insert p2' (not v) .  M.insert p' v $ crates'
      where
        p' = p + dirPoint d
        -- q | moved = q - V2 1 0
        --   | otherwise = q + v2 1 0
        -- q' = q + dirPoint d


