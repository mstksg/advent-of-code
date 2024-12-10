Both of these today involve consuming queues, but the nature of the queues are
different. For part 1, we consume two queues: the queue of gaps from left to
right, and the queue of files from right to left. For part 2, we consume the
queue of file blocks from right to left.

We can actually consume the queues in both cases directly into their checksum
without going through an intermediate structure, which is kind of convenient
too.

First, let's parse the list of numbers into a usable state: for gaps, an
`IntMap` of positions to gap sizes, and for file blocks, an `IntMap` of
positions to id's and fids.

```haskell
toDiskState :: [a] -> [Int] -> (IntMap Int, IntMap (a, Int))
toDiskState fids =
      IM.mapEither splitter
    . IM.fromList
    . snd
    . mapAccumL go 0
    . zip (intersperse Nothing (Just <$> fids))
  where
    go i (mfid, len) = (i + len, (i, (mfid, len)))
    splitter (mfid, len) = case mfid of
      Nothing -> Left len
      Just fid -> Right (fid, len)
```

For part 1, the behavior of the queues is non-trivial so it's helpful to write
it using explicit recursion. The first queue is the queue of gaps (which we
push-back on with a smaller gap length) and the second queue is the queue of
reversed single (file slot index, file id) that we pop one-by-one. We also
short-circuit to the end if our forward gap indices move past our backwards
file indices.

```haskell
fillGaps
  :: [(Int, Int)]   -- ^ list of (gap starting Index, gap length) left-to-right
  -> [(Int, Int)]   -- ^ list of (single file slot index, file id) right-to-left
  -> Int
fillGaps [] ends = sum $ map (uncurry (*)) ends
fillGaps _ [] = 0
fillGaps ((gapI, gapLen):gaps) ((endI, fid):ends)
  | endI > gapI -> gapI * fid + fillGaps (addBack gaps) ends
  | otherwise -> endI * fid + sum (map (uncurry (*)) ends)
  where
    addBack
      | gapLen == 1 = id
      | otherwise = ((gapI + 1, gapLen - 1) :)

part1 :: IntMap Int -> IntMap (Int, Int) -> Int
part1 gaps files =
  fillGaps
    (IM.toList gaps)
    [ (i, fid)
    | (i0, (fid, len)) <- IM.toDescList dsFiles
    , i <- take len $ iterate (subtract 1) (i0 + len - 1)
    ]
```

For part 2, our queue consumption is pretty typical, with no re-push or
short-circuiting. We just move through every single file in reverse once, so it
can be captured as a `mapAccumL`: a stateful map over the backwards file
blocks, where state is the empty slot candidates.

```haskell
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

part2 :: IntMap Int -> IntMap (Int, Int) -> Int
part2 gaps files = sum . snd . mapAccumL moveBlock gaps $ IM.toDescList files
```
