-- |
-- Module      : AOC2022.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day24 (
  day24a,
  day24b,
)
where

import AOC.Common.Point (
  Dir (..),
  Point,
  boundingBox,
  cardinalNeighbs,
  dirPoint,
  mannDist,
  parseAsciiMap,
 )
import AOC.Common.Search (aStar)
import AOC.Solver ((:~>) (..))
import Control.DeepSeq (NFData)
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import GHC.Generics (Generic)
import Linear.V2 (V2 (..))
import Safe (lastMay)

parseChar :: Char -> Maybe Dir
parseChar '>' = Just East
parseChar '<' = Just West
parseChar '^' = Just South
parseChar 'v' = Just North
parseChar _ = Nothing

data MapState = MS
  { time :: !Int
  , pos :: !Point
  }
  deriving stock (Eq, Show, Ord, Generic)

instance NFData MapState

-- | Assume corner at (0,0).
solve :: Int -> NEMap Point Dir -> Maybe Int
solve n bmap =
  flip evalStateT (MS 0 origin) $
    sum <$> traverse oneLeg (take n (cycle [goal, origin]))
  where
    oneLeg :: Point -> StateT MapState Maybe Int
    oneLeg dest = StateT $ \ms ->
      traverse lastMay
        =<< aStar (mannDist dest . pos) expander ms ((== dest) . pos)
    V2 _ maxes = boundingBox $ NEM.keys bmap
    cyclePeriod = product (maxes + 1)
    goal = maxes + V2 0 1
    origin = V2 0 (-1)
    bHist =
      Seq.fromList . map S.fromList . take cyclePeriod $
        iterate stepBs (toList (NEM.keys bmap))
      where
        stepBs =
          zipWith
            (\d p -> mod <$> (p + dirPoint d) <*> (maxes + 1))
            (toList bmap)
    validPos =
      S.insert goal . S.insert origin . S.fromList $
        sequence $
          enumFromTo <$> 0 <*> maxes
    expander (MS t p) = M.fromAscList [(MS (t + 1) p', 1) | p' <- S.toList cands]
      where
        bs = bHist `Seq.index` ((t + 1) `mod` cyclePeriod)
        cands =
          (S.fromList (p : cardinalNeighbs p) `S.intersection` validPos)
            `S.difference` bs

day24 :: Int -> NEMap Point Dir :~> Int
day24 n =
  MkSol
    { sParse = NEM.nonEmptyMap . parseAsciiMap parseChar
    , sShow = show
    , sSolve = solve n . NEM.mapKeys (subtract 1)
    }

day24a :: NEMap Point Dir :~> Int
day24a = day24 1

day24b :: NEMap Point Dir :~> Int
day24b = day24 3
