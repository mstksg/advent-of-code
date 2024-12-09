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
import Control.Monad.Trans.State (evalState, runState, state)
import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'), find)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (intersperse)
import Data.Traversable (for)
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
              (newGaps, newFiles) = flip runState endFiles . fmap concat . for (IM.toAscList dsGaps) $ \(i0, gapLen) -> state \ends ->
                let (fillMe, rest) = splitAt gapLen ends
                    (fillToTheMiddle, pastMiddle) = span (\((endI, _), gapI) -> endI > gapI) $ zip fillMe [i0 ..]
                 in ((\((_, fid), i) -> (i, fid)) <$> fillToTheMiddle, (fst <$> pastMiddle) ++ rest)
           in sum . map (uncurry (*)) $ newGaps ++ newFiles
    }

advanceBlock :: DiskState a -> (Int, (a, Int)) -> DiskState a
advanceBlock ds (i, (fid, fileLen)) =
  maybe ds update . find ((>= fileLen) . snd) . IM.toAscList . IM.takeWhileAntitone (< i) $ dsGaps ds
  where
    update (gapI, gapLen) =
      DS
        { dsGaps = IM.union reGap . IM.delete gapI $ dsGaps ds
        , dsFiles = IM.insert gapI (fid, fileLen) . IM.delete i $ dsFiles ds
        }
      where
        reGap = IM.filter (> 0) $ IM.singleton (gapI + fileLen) (gapLen - fileLen)

checksum :: IM.IntMap (Int, Int) -> Int
checksum = sum . map go . IM.toList
  where
    go (i, (v, len)) = sum . map (* v) . take len $ [i ..]

day09b :: [Int] :~> Int
day09b =
  MkSol
    { sParse = sParse day09a
    , sShow = show
    , sSolve =
        noFail \ls ->
          let ds0 = toDiskState [0 ..] ls
           in checksum . dsFiles . foldl' advanceBlock ds0 $ IM.toDescList (dsFiles ds0)
    }
