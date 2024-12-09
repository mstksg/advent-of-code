{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day09 (
  day09a,
  day09b,
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

day09a :: _ :~> _
day09a =
  MkSol
    { sParse =
        noFail $
          map digitToInt
    , sShow = show
    , sSolve =
        noFail \ls ->
          let board =
                M.fromList . mapMaybe (\(i, x) -> (i :: Int,) <$> x) . zip [0 ..] . concat $
                  zipWith replicate ls (intersperse Nothing $ Just <$> [0 ..])
              maxIx = last $ M.keys board
              allIx = [0 .. maxIx]
              (finalBoard, _) = flip loopMaybe (board, 0) \(ps, n) -> traceShow n do
                let (iLast, pLast) = M.findMax ps
                firstGap <- find (`M.notMember` ps) allIx
                guard $ firstGap /= (iLast + 1)
                pure (M.insert firstGap pLast $ M.delete iLast ps, n + 1)
           in sum . map (uncurry (*)) $ M.toList finalBoard
    }

day09b :: _ :~> _
day09b =
  MkSol
    { sParse = sParse day09a
    , sShow = show
    , sSolve =
        noFail \ls ->
          let board = M.fromList $
                flip evalState 0 $
                  for (zip ls (intersperse Nothing $ Just <$> [0 ..])) \(len, fid) -> state \i ->
                    ((i, (fid, len)), i + len)
              emptyBoard :: M.Map Int Int
              emptyBoard = M.mapMaybe (\(fid, i) -> case fid of Nothing -> Just i; Just _ -> Nothing) board
              fullBoard :: M.Map Int (Int, Int)
              fullBoard = M.mapMaybe (\(fid, i) -> (,i) <$> fid) board
              (_, _, finalFull) = flip loopMaybe (S.empty, emptyBoard, fullBoard) \(moved, psEmpty, psFull) -> do
                (gapI, gapLen, moveMeI, moveMe, moveMeLen) <- flip firstJust (M.toDescList psFull) \(i, (fid, len)) -> do
                  guard $ fid `S.notMember` moved
                  (gapI, gapLen) <- find ((len <=) . snd) (M.toAscList psEmpty)
                  guard $ gapI < i
                  pure (gapI, gapLen, i, fid, len)
                let reAddEmpty
                      | gapLen == moveMeLen = id
                      | otherwise = M.insert (gapI + moveMeLen) (gapLen - moveMeLen)
                    newEmpty = reAddEmpty $ M.delete gapI psEmpty
                    newFull = M.insert gapI (moveMe, moveMeLen) . M.delete moveMeI $ psFull
                    newMoved = S.insert moveMe moved
                pure (newMoved, newEmpty, newFull)
              checksum (i, (v, len)) = sum . map (* v) . take len $ [i ..]
           in sum . map checksum $ M.toList finalFull
    }
