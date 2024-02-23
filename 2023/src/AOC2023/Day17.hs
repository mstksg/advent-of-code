{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
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
module AOC2023.Day17 (
  day17a,
  day17b,
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

day17 :: Int -> Int -> Map Point Int :~> Int
day17 a b =
  MkSol
    { sParse = noFail $ parseAsciiMap digitToIntSafe
    , sShow = show
    , sSolve = \mp -> do
        V2 p1 p2 <- boundingBox' (M.keys mp)
        let grow (lastDir, p) =
              M.fromList
                [ ((Just d, p'), sum (snd <$> steps))
                | d <- toList allDir
                , case lastDir of
                    Nothing -> True
                    Just d' -> d /= d' && d /= (d' <> South)
                , steps <- drop a $ inits do
                    i <- [1 .. b]
                    let p' = p + i *^ dirPoint d
                    cost <- maybeToList $ M.lookup p' mp
                    pure (p', cost)
                , (p', _) <- maybeToList $ lastMay steps
                ]
        fst <$> aStar ((`mannDist` p2) . snd) grow (Nothing, p1) ((== p2) . snd)
    }

day17a :: Map Point Int :~> Int
day17a = day17 1 3

day17b :: Map Point Int :~> Int
day17b = day17 4 10
