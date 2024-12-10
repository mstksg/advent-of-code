-- |
-- Module      : AOC2024.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day09 (
  day09a,
  day09b,
)
where

import AOC.Solver (noFail, type (:~>) (..))
import Control.DeepSeq (NFData)
import Data.Char (digitToInt)
import Data.Foldable (find)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (intersperse, mapAccumL)
import GHC.Generics (Generic, Generically (..))

data DiskState a = DS
  { dsGaps :: !(IntMap Int)
  , dsFiles :: !(IntMap (a, Int))
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (Semigroup, Monoid) via (Generically (DiskState a))

toDiskState :: [a] -> [Int] -> DiskState a
toDiskState fids =
  uncurry DS
    . IM.mapEither splitter
    . IM.fromList
    . snd
    . mapAccumL go 0
    . zip (intersperse Nothing (Just <$> fids))
  where
    go i (mfid, len) = (i + len, (i, (mfid, len)))
    splitter (mfid, len) = case mfid of
      Nothing -> Left len
      Just fid -> Right (fid, len)

fillGaps :: [(Int, Int)] -> [(Int, Int)] -> Int
fillGaps = \case
  [] -> sum . map (uncurry (*))
  (gapI, gapLen) : gaps -> \case
    [] -> 0
    (endI, fid) : ends
      | endI > gapI ->
          let addBack
                | gapLen == 1 = id
                | otherwise = ((gapI + 1, gapLen - 1) :)
           in gapI * fid + fillGaps (addBack gaps) ends
      | otherwise -> endI * fid + sum (map (uncurry (*)) ends)

day09a :: [Int] :~> Int
day09a =
  MkSol
    { sParse =
        noFail $
          map digitToInt
    , sShow = show
    , sSolve =
        noFail \ls ->
          let DS{..} = toDiskState [0 ..] ls
           in fillGaps
                (IM.toAscList dsGaps)
                [ (i, fid)
                | (i0, (fid, len)) <- IM.toDescList dsFiles
                , i <- take len $ iterate (subtract 1) (i0 + len - 1)
                ]
    }

moveBlock :: IntMap Int -> (Int, (Int, Int)) -> (IntMap Int, Int)
moveBlock gaps (i, (fid, fileLen)) = (gaps', hereContrib)
  where
    foundGap = find ((>= fileLen) . snd) . IM.toAscList $ IM.takeWhileAntitone (< i) gaps
    hereContrib = fid * ((fileLen * (fileLen + 1)) `div` 2 + fileLen * (maybe i fst foundGap - 1))
    gaps' = case foundGap of
      Nothing -> gaps
      Just (gapI, gapLen) ->
        let addBack
              | gapLen > fileLen = IM.insert (gapI + fileLen) (gapLen - fileLen)
              | otherwise = id
         in addBack . IM.delete gapI $ gaps

day09b :: [Int] :~> Int
day09b =
  MkSol
    { sParse = sParse day09a
    , sShow = show
    , sSolve =
        noFail \ls ->
          let DS{..} = toDiskState [0 ..] ls
           in sum . snd . mapAccumL moveBlock dsGaps $ IM.toDescList dsFiles
    }
