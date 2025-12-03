{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2025.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
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
module AOC2025.Day03 where 
  -- (
  -- day03a,
  -- day03b,
  -- go2,
  -- go12
-- )
-- where

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

day03a :: _ :~> _
day03a =
  MkSol
    { sParse =
        noFail $
          map (map digitToInt) . lines
    , sShow = show
    , sSolve =
        noFail $
          sum . map go2
    }
  where
go2 xs = maximum
  [ x * 10 + y
  | x: ys <- tails xs
  , y:_ <- tails ys
  ]

day03b :: _ :~> _
day03b =
  MkSol
    { sParse = sParse day03a
    , sShow = show
    , sSolve =
            fmap sum . traverse (readMaybe @Int . map intToDigit <=< listToMaybe . evalStateT runItAll)
          -- sum . map go12
    }
  where

    go xs = find (inPat xs) (candidates xs)
      where
        inPat _ [] = True
        inPat [] (_:_) = False
        inPat (x:xs') (c:cs)
          | x == c = inPat xs' cs
          | otherwise = inPat xs' (c:cs)

pickAtN :: Int -> [Int] -> [[Int]]
pickAtN n xs = [ xs'
                | x:xs' <- tails xs
               , x == n
               ]

descendPicks :: [Int] -> [(Int, [Int])]
descendPicks xs = do 
  n <- reverse [1..9]
  picked <- pickAtN n xs
  pure (n, picked)

runItAll :: StateT [Int] [] [Int]
runItAll = replicateM 12 (StateT descendPicks)

candidates :: [Int] -> [[Int]]
candidates = selections . reverse . sort
-- -- pick max candidates -- and position
-- -- pickMax :: Seq a -> Maybe (Int, a)

-- maxIx :: [a] -> Maybe
-- go12' xs = _
--   where
--     firstFound n = 

-- go12 = undefined
-- 9869755494885236452767647548826258555668838497687557968652485746757835445145546687436356799481798587

selections xs =
      [ [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12]
      | x1: xs1 <- tails xs
      , x2: xs2 <- tails xs1
      , x3: xs3 <- tails xs2
      , x4: xs4 <- tails xs3
      , x5: xs5 <- tails xs4
      , x6: xs6 <- tails xs5
      , x7: xs7 <- tails xs6
      , x8: xs8 <- tails xs7
      , x9: xs9 <- tails xs8
      , x10: xs10 <- tails xs9
      , x11: xs11 <- tails xs10
      , x12: xs12 <- tails xs11
      ]

go12 xs = traceShowId . read . map intToDigit $ maximum
      [ [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12]
      | x1: xs1 <- tails xs
      , x2: xs2 <- tails xs1
      , x3: xs3 <- tails xs2
      , x4: xs4 <- tails xs3
      , x5: xs5 <- tails xs4
      , x6: xs6 <- tails xs5
      , x7: xs7 <- tails xs6
      , x8: xs8 <- tails xs7
      , x9: xs9 <- tails xs8
      , x10: xs10 <- tails xs9
      , x11: xs11 <- tails xs10
      , x12: xs12 <- tails xs11
      ]
