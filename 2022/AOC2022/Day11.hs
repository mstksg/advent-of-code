{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2022.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
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
module AOC2022.Day11 (
  day11a,
  day11b,
)
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import qualified Data.IntMap as IM
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

data MonkeyOp = Add | Mul
  deriving stock (Show)

data MonkeyData = MD
  { mdItems :: [Int]
  , mdTrans :: (MonkeyOp, Maybe Int)
  -- ^ false is add
  , mdCond :: Int
  , mdTrue :: Int
  , mdFalse :: Int
  }
  deriving stock (Show)

parseMonkey :: String -> Maybe MonkeyData
parseMonkey blob = do
  [_, a, b, c, d, e] <- pure $ lines blob
  mdItems <- readAll $ words (clearOut (not . isDigit) a)
  [_, x, y] <- pure $ words . drop 1 $ dropWhile (/= '=') b
  let mdTrans = (if x == "*" then Mul else Add, readMaybe y)
  mdCond <- readMaybe $ clearOut (not . isDigit) c
  mdTrue <- readMaybe $ clearOut (not . isDigit) d
  mdFalse <- readMaybe $ clearOut (not . isDigit) e
  pure MD{..}

stepState ::
  MonkeyData ->
  [Int] ->
  IntMap (Seq.Seq Int) ->
  IntMap (Seq.Seq Int)
stepState mdat xs = IM.unionWith (flip (<>)) newDat
  where
    newDat =
      IM.fromListWith
        (<>)
        [ (j, Seq.singleton x'')
        | x <- xs
        , let x' = case mdTrans mdat of
                (Add, Nothing) -> x + x
                (Add, Just q) -> x + q
                (Mul, Nothing) -> x * x
                (Mul, Just q) -> x * q
              x'' = x' `div` 3
              j =
                if (x'' `div` mdCond mdat) == 0
                  then mdTrue mdat
                  else mdFalse mdat
        ]

stepRound ::
  [MonkeyData] ->
  IntMap (Seq.Seq Int) ->
  (IntMap (Seq.Seq Int), IntMap Int)
stepRound mds i0 = second IM.fromList $ mapAccumL go i0 (zip [0 ..] mds)
  where
    go smap (mIx, md) = (smap', (mIx, Seq.length toProcess))
      where
        toProcess = IM.findWithDefault Seq.empty mIx smap
        smap' = IM.delete mIx $ stepState md (toList toProcess) smap

-- Monkey 0:
--   Starting items: 63, 84, 80, 83, 84, 53, 88, 72
--   Operation: new = old * 11
--   Test: divisible by 13
--     If true: throw to monkey 4
--     If false: throw to monkey 7

day11a :: _ :~> _
day11a =
  MkSol
    { sParse = traverse parseMonkey . splitOn "\n\n"
    , sShow = show
    , sSolve = noFail \mds ->
        let initMap = IM.fromList . zipWith (\i md -> (i, Seq.fromList $ mdItems md)) [0 ..] $ mds
         in flip evalState initMap . replicateM 20 $ state $ swap . stepRound mds
    }

day11b :: _ :~> _
day11b =
  MkSol
    { sParse = sParse day11a
    , sShow = show
    , sSolve = Just
    }
