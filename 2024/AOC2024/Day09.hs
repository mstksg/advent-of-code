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
import Control.Monad.State (MonadState (state), evalState)
import Data.Char (digitToInt)
import Data.Foldable (find)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (intersperse)
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
    . flip evalState 0
    . traverse go
    . zip (intersperse Nothing (Just <$> fids))
  where
    go (mfid, len) = state \i -> ((i, (mfid, len)), i + len)
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
              endFiles :: [(Int, Int)]
              endFiles =
                [ (i, fid)
                | (i0, (fid, len)) <- IM.toDescList dsFiles
                , i <- take len $ iterate (subtract 1) (i0 + len - 1)
                ]
           in fillGaps (IM.toAscList dsGaps) endFiles
    }

moveBlocks :: IntMap Int -> [(Int, (Int, Int))] -> Int
moveBlocks gaps = \case
  [] -> 0
  (i, (fid, fileLen)) : rest ->
    let foundGap = find ((>= fileLen) . snd) . IM.toAscList $ IM.takeWhileAntitone (< i) gaps
        hereContrib = sum . map (* fid) . take fileLen $ [maybe i fst foundGap ..]
        gaps' = case foundGap of
          Nothing -> gaps
          Just (gapI, gapLen) ->
            let reGap = IM.filter (> 0) $ IM.singleton (gapI + fileLen) (gapLen - fileLen)
             in IM.union reGap . IM.delete gapI $ gaps
     in hereContrib + moveBlocks gaps' rest

day09b :: [Int] :~> Int
day09b =
  MkSol
    { sParse = sParse day09a
    , sShow = show
    , sSolve =
        noFail \ls ->
          let DS{..} = toDiskState [0 ..] ls
           in moveBlocks dsGaps $ IM.toDescList dsFiles
    }
