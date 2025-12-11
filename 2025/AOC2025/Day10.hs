{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2025.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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
module AOC2025.Day10 (
  day10a,
  day10b,
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

day10a :: _ :~> _
day10a =
  MkSol
    { sParse =
        noFail $
          map parseMe . lines
    , sShow = show
    , sSolve =
        noFail $ sum . map go
    }
  where
    parseMe :: String -> ([Bool], [[Int]], [Int])
    parseMe ('[':xs) = (map (== '#') a, map read . splitOn "," . init . tail <$> init ps, map read . splitOn "," . init . tail $ last ps)
      where
        (a, ']':bs) = span (/= ']') xs
        ps = words bs
    go :: ([Bool], [[Int]], [Int]) -> Int
    go (targ, buttons, _) = minimum
        [ length onButts
          | onButts <- filterM (\_ -> [False, True]) buttons
        , foldr symmetricDifference S.empty (S.fromList <$> onButts) == targSet
        ]
      where
        targSet = S.fromList $ map fst $ filter snd $ zip [0..] targ
      -- traceShow vecs 3
      -- where
      --   vecs = flip map buttons \ixes -> zipWith (\i _ -> i `S.member` S.fromAscList ixes) [0..] targ

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference x y = (x <> y) `S.difference` (x `S.intersection` y)

-- [.##.]
-- (...#)
-- (.#.#)
-- (..#.)
-- (..##)
-- (#.#.)
-- (##..)
-- {3,5,4,7}
--
-- ....## a   .
-- .#...# b = #
-- ..###. c   #
-- ##.#.. d   .
--        e
--        f
--
--         e+f = 0 mod 2
--   b+      f = 1 mod 2
--     c+d+e   = 1 mod 2
-- a+b+  d     = 0 mod 2

-- [.##.] (...#) (.#.#) (..#.) (..##) (#.#.) (##..) {3,5,4,7}

-- [.##.] (3)    (1,3) (2)     (2,3)  (0,2)  (0,1) {3,5,4,7}
-- [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
-- [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

day10b :: _ :~> _
day10b =
  MkSol
    { sParse = sParse day10a
    , sShow = show
    , sSolve =
        noFail $ sum . map go
    }
  where
    go :: ([Bool], [[Int]], [Int]) -> Int
    go (_, buttons, targ) = minimum $ flip evalStateT (0 <$ targMap) $ do
          tots <- traverse (goo . IS.fromList) buttons
          -- tots <- traverse (goo . IS.fromList) $ sortOn (Down . length) buttons
          guard =<< gets (== targMap)
          traceM (show tots)
          pure $ sum tots
      where
        goo :: IntSet -> StateT (IntMap Int) [] Int
        goo button = StateT $ \soFar -> traceShowId $
          takeWhile (and . IM.intersectionWith (>=) targMap . snd) $
              iterate (\(!i, mp) -> (i + 1, IM.unionWith (+) buttonMap mp)) (0, soFar)
          -- let leftOver = IM.intersectionWith (-) targMap soFar
          where
            buttonMap = IM.fromSet (const 1) button
        targMap = IM.fromList $ zip [0..] targ

-- (...#)
-- (.#.#)
-- (..#.)
-- (..##)
-- (#.#.)
-- (##..)
-- {3547}

--         e+f = 3
--   b+      f = 5
--     c+d+e   = 4
-- a+b+  d     = 7

-- ....## a   3
-- .#...# b = 5
-- ..###. c   4
-- ##.#.. d   7
--        e
--        f
