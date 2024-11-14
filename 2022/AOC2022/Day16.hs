{-# LANGUAGE TypeApplications #-}

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
  -- day16b,
)
where

import AOC.Common (clearOut)
import AOC.Common.Search (aStar)
import AOC.Solver ((:~>) (..))
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Char (isDigit, isUpper)
import Data.Foldable (toList)
import Data.List (uncons)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Linear (V1 (..), V2 (..))
import Text.Read (readMaybe)

parseLine :: String -> Maybe (String, (Int, Set String))
parseLine xs = do
  (a, bs) <- uncons $ words $ clearOut (not . isUpper) (tail xs)
  r <- readMaybe $ clearOut (not . isDigit) xs
  pure (a, (r, S.fromList bs))

data PuzzState f = PuzzState
  { time :: !Int
  , pos :: !(f String)
  , opened :: !(Set String)
  }
  deriving stock (Generic)

instance NFData (f String) => NFData (PuzzState f)

deriving stock instance Show (f String) => Show (PuzzState f)

deriving stock instance Eq (f String) => Eq (PuzzState f)

deriving stock instance Ord (f String) => Ord (PuzzState f)

searchPuzzle ::
  forall f.
  (Applicative f, Traversable f, Ord (f String)) =>
  Int ->
  Map String (Int, Set String) ->
  Maybe (Int, [PuzzState f])
searchPuzzle maxTime mp =
  first reCost
    <$> aStar
      (oneTickCost . opened)
      expand
      (PuzzState 1 (pure "AA") S.empty)
      (\(PuzzState t _ o) -> t >= maxTime || S.size o >= S.size pipesWithFlow)
  where
    pipesWithFlow = M.keysSet $ M.filter ((> 0) . fst) mp
    reCost x = (oneTickCost S.empty * maxTime) - x
    oneTickCost :: Set String -> Int
    oneTickCost opened = sum . map fst . toList $ mp `M.withoutKeys` opened
    expand :: PuzzState f -> Map (PuzzState f) Int
    expand (PuzzState t ps o) =
      M.fromList
        [ (PuzzState (t + 1) newPos (S.unions newSeen), oneTickCost o)
        | newPosSeen <- traverse go ps -- it's always traverse
        , let newPos = fst <$> newPosSeen
              newSeen = snd <$> newPosSeen
        ]
      where
        go :: String -> [(String, Set String)]
        go p = stayHereAndOpen <|> moveToAnother
          where
            moveToAnother = do
              p' <- toList $ snd (mp M.! p)
              pure (p', o)
            stayHereAndOpen = do
              guard $ p `S.member` pipesWithFlow && p `S.notMember` o
              pure (p, S.insert p o)

day16 ::
  forall f.
  (Applicative f, Traversable f, Ord (f String)) =>
  Int ->
  Map String (Int, Set String) :~> Int
day16 t =
  MkSol
    { sParse = fmap M.fromList . traverse parseLine . lines
    , sShow = show
    , sSolve = fmap fst . searchPuzzle @f t
    }

day16a :: Map String (Int, Set String) :~> Int
day16a = day16 @V1 30

day16b :: Map String (Int, Set String) :~> Int
day16b = day16 @V2 26
