-- |
-- Module      : AOC2022.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day16 (
  day16a,
  day16b,
)
where

import AOC.Common (clearOut)
import AOC.Common.Search (bfs)
import AOC.Solver ((:~>) (..))
import Data.Char (isDigit, isUpper)
import Data.Foldable (toList)
import Data.Functor.Foldable (hylo)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (uncons)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Linear (V1 (..), V2 (..))
import Safe (maximumMay)
import Text.Read (readMaybe)

parseLine :: String -> Maybe (Text, (Int, Set Text))
parseLine xs = do
  (a, bs) <- uncons $ words $ clearOut (not . isUpper) (drop 1 xs)
  r <- readMaybe $ clearOut (not . isDigit) xs
  pure (T.pack a, (r, S.fromList (map T.pack bs)))

data SearchState = SearchState
  { ssCur :: !Text
  , ssTimeLeft :: !Int
  , ssOpened :: !IntSet
  , ssScore :: !Int
  }
  deriving stock (Show)

data StepF b = StepF SearchState [b]
  deriving stock (Functor)

bestPerOpened :: Int -> Map Text (Int, Set Text) -> Map IntSet Int
bestPerOpened budget mp =
  M.fromListWith max [(ssOpened s, ssScore s) | s <- visited]
  where
    valves = M.keys $ M.filter ((> 0) . fst) mp
    neighbors n = foldMap snd (M.lookup n mp)
    dists =
      flip M.fromSet (S.insert "AA" (S.fromList valves)) $ \v ->
        M.fromList [(u, length path) | u <- valves, Just path <- [bfs neighbors v (== u)]]
    flowOf v = fst (mp M.! v)
    visited =
      hylo
        (\(StepF s bs) -> s : concat bs)
        (\s -> StepF s (nexts s))
        SearchState{ssCur = "AA", ssTimeLeft = budget, ssOpened = IS.empty, ssScore = 0}
    nexts :: SearchState -> [SearchState]
    nexts s@(SearchState cur timeLeft opened _) =
      [ SearchState
          { ssCur = v
          , ssTimeLeft = timeLeft'
          , ssOpened = IS.insert i opened
          , ssScore = ssScore s + timeLeft' * flowOf v
          }
      | (i, v) <- zip [0 ..] valves
      , i `IS.notMember` opened
      , let timeLeft' = timeLeft - (here M.! v) - 1
      , timeLeft' > 0
      ]
      where
        here = dists M.! cur

sumIfDisjoint :: [(IntSet, Int)] -> Maybe Int
sumIfDisjoint xs = foldr step (const (Just 0)) xs IS.empty
  where
    step (opened, score) rest seen
      | IS.disjoint seen opened = (score +) <$> rest (seen <> opened)
      | otherwise = Nothing

day16 ::
  forall f.
  (Applicative f, Traversable f) =>
  Int ->
  Map Text (Int, Set Text) :~> Int
day16 budget =
  MkSol
    { sParse = fmap M.fromList . traverse parseLine . lines
    , sShow = show
    , sSolve =
        maximumMay
          . mapMaybe (sumIfDisjoint . toList)
          . sequence
          . pure @f
          . M.toList
          . bestPerOpened budget
    }

day16a :: Map Text (Int, Set Text) :~> Int
day16a = day16 @V1 30

day16b :: Map Text (Int, Set Text) :~> Int
day16b = day16 @V2 26
