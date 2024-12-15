-- |
-- Module      : AOC2024.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day15 (
  day15a,
  day15b,
)
where

import AOC.Common.Point (Dir (North, South), Point, dirPoint, parseAsciiMap, parseDir)
import AOC.Solver (type (:~>) (..))
import Data.Bifunctor (Bifunctor (second))
import Data.Bool (bool)
import Data.Foldable (foldl', toList)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2 (V2 (..))

-- | Return walls, crates, and player
splitMap :: Map Point Char -> Maybe (Set Point, Set Point, Point)
splitMap =
  retuple
    . second (M.mapEither (bool (Left ()) (Right ())))
    . M.mapEither (maybe (Left ()) Right)
    . M.mapMaybe toEntity
  where
    toEntity =
      flip M.lookup $
        M.fromList
          [ ('@', Nothing)
          , ('#', Just False)
          , ('O', Just True)
          ]
    retuple (player, (walls, crates)) = (M.keysSet walls,M.keysSet crates,) <$> S.lookupMin (M.keysSet player)

day15a :: (Map Point Char, [Dir]) :~> Int
day15a =
  MkSol
    { sParse = \xs -> case splitOn "\n\n" xs of
        [x, d] -> (parseAsciiMap Just x,) <$> traverse parseDir (filter (/= '\n') d)
        _ -> Nothing
    , sShow = show
    , sSolve = \(mp, path) -> do
        (walls, crates, person) <- splitMap mp
        pure . sum . map score . toList . snd $ foldl' (step walls) (person, crates) path
    }

score :: Num a => V2 a -> a
score (V2 x y) = 100 * y + x

step :: Set Point -> (Point, Set (V2 Int)) -> Dir -> (Point, Set (V2 Int))
step walls (person, crates) d
  | person' `S.member` walls = (person, crates)
  | person' `S.member` crates = maybe (person, crates) (person',) $ tryMove person' crates
  | otherwise = (person', crates)
  where
    person' = person + dirPoint d
    tryMove p crates' =
      commit
        <$> if
          | p' `S.member` walls -> Nothing
          | p' `S.member` crates' -> tryMove p' crates'
          | otherwise -> Just crates'
      where
        p' = p + dirPoint d
        commit = S.delete p . S.insert p'

day15b :: (Map Point Char, [Dir]) :~> Int
day15b =
  MkSol
    { sParse = sParse day15a
    , sShow = show
    , sSolve = \(mp, path) -> do
        (walls, crates, person) <- splitMap mp
        let walls' = S.fromList . concatMap doublePoint . toList $ walls
            crates' = M.fromList . concatMap (flip zip [False, True] . doublePoint) . toList $ crates
            person' = person * V2 2 1
        pure $ sum . map score . M.keys . M.filter not . snd $ foldl' (step2 walls') (person', crates') path
    }
  where
    doublePoint (V2 x y) = ($ y) <$> [V2 (2 * x), V2 (2 * x + 1)]

step2 :: Set Point -> (V2 Int, Map (V2 Int) Bool) -> Dir -> (V2 Int, Map (V2 Int) Bool)
step2 walls (person, crates) d
  | person' `S.member` walls = (person, crates)
  | otherwise = case M.lookup person' crates of
      Just lr -> maybe (person, crates) (person',) $ tryMove person' crates lr
      Nothing -> (person', crates)
  where
    person' = person + dirPoint d
    tryMove p crates' moved = do
      crates'' <- tryMoveSingle p crates' moved
      if d `elem` [North, South]
        then tryMoveSingle (bump moved p) crates'' (not moved)
        else pure crates''
    tryMoveSingle p crates' moved =
      commit
        <$> if p' `S.member` walls
          then Nothing
          else case M.lookup p' crates' of
            Just lr -> tryMove p' crates' lr
            Nothing -> Just crates'
      where
        p' = p + dirPoint d
        commit = M.delete p . M.insert p' moved
    bump = \case
      False -> (+ V2 1 0)
      True -> subtract (V2 1 0)
