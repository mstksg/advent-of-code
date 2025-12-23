-- |
-- Module      : AOC2022.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
module AOC2022.Day20 (
  day20a,
  day20b,
)
where

import AOC.Common (readAll, (!!!))
import AOC.Solver ((:~>) (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

mix :: Seq (Int, Int) -> Seq (Int, Int)
mix = go 0
  where
    go i xs = case vPostXs of
      (_, v) :<| postXs ->
        let newIx = (Seq.length preXs + v) `mod` (Seq.length xs - 1)
         in go (i + 1) $ Seq.insertAt newIx (i, v) (preXs <> postXs)
      _ -> xs
      where
        (preXs, vPostXs) = Seq.spanl ((/= i) . fst) xs

grove :: Seq Int -> Maybe Int
grove xs = do
  i <- Seq.elemIndexL 0 xs
  let ixs = (`mod` Seq.length xs) . (+ i) <$> [1000, 2000, 3000]
  vs <- traverse (`Seq.lookup` xs) ixs
  pure $ sum vs

day20a :: [Int] :~> Int
day20a =
  MkSol
    { sParse = readAll . lines
    , sShow = show
    , sSolve = grove . fmap snd . mix . Seq.fromList . zip [0 ..]
    }

magic :: Int
magic = 811589153

day20b :: [Int] :~> Int
day20b =
  MkSol
    { sParse = readAll . lines
    , sShow = show
    , sSolve =
        grove
          . fmap snd
          . (!!! 10)
          . iterate mix
          . Seq.fromList
          . zip [0 ..]
          . map (* magic)
    }
