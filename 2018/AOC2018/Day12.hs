-- |
-- Module      : AOC2018.Day12
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
module AOC2018.Day12 (
  day12a,
  day12b,
) where

import AOC.Common (asString, (!!!))
import AOC.Solver ((:~>) (..))
import Data.Finite (Finite, finites)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Ctx = Set (Finite 5)
type Pos = Int

step ::
  Set Ctx ->
  Set Pos ->
  Set Pos
step ctxs w0 =
  S.fromDistinctAscList
    . filter go
    $ [S.findMin w0 - 2 .. S.findMax w0 + 2]
  where
    go i = neighbs `S.member` ctxs
      where
        neighbs = S.fromDistinctAscList . flip filter finites $ \j ->
          (i - 2 + fromIntegral j) `S.member` w0

findLoop ::
  Set Ctx ->
  Set Pos ->
  (Int, Int, Int) -- time to loop, loop size, loop incr
findLoop ctxs w0 = go (M.singleton w0 (0, 0)) 1 w0
  where
    go !seen !i !w = case M.lookup w'Norm seen of
      Nothing -> go (M.insert w'Norm (mn, i) seen) (i + 1) w'
      Just (seenMn, seenI) -> (seenI, i - seenI, mn - seenMn)
      where
        w' = step ctxs w
        (mn, w'Norm) = normalize w'
    normalize w = (mn, S.map (subtract mn) w)
      where
        mn = S.findMin w

stepN ::
  Int ->
  Set Pos ->
  Set Ctx ->
  Set Pos
stepN n w ctx =
  goN extra
    . S.map (+ (loopIncr * looped))
    . goN ttl
    $ w
  where
    goN m = (!!! m) . iterate (step ctx)
    (ttl, loopSize, loopIncr) = findLoop ctx w
    (looped, extra) = (n - ttl) `divMod` loopSize

day12a :: (Set Pos, Set Ctx) :~> Int
day12a =
  MkSol
    { sParse = parse
    , sShow = show
    , sSolve = \(w, ctx) -> Just . sum $ iterate (step ctx) w !!! 20
    }

day12b :: (Set Pos, Set Ctx) :~> Int
day12b =
  MkSol
    { sParse = parse
    , sShow = show
    , sSolve = Just . sum . uncurry (stepN 50000000000)
    }

parse :: String -> Maybe (Set Pos, Set Ctx)
parse s = (makeState stateStr,) <$> makeCtxs ctxStr
  where
    (stateStr, ctxStr) = span (/= '\n') s

makeState :: String -> Set Pos
makeState =
  M.keysSet
    . M.filter (== '#')
    . M.fromList
    . zip [0 ..]
    . filter (`elem` asString "#.")

makeCtxs :: String -> Maybe (Set Ctx)
makeCtxs str = do
  pairs <- traverse parseCtxLine . filter (not . null) . lines $ str
  pure . M.keysSet . M.filter id . M.fromList $ pairs
  where
    parseCtxLine l = case splitAt 5 . map (== '#') . filter (`elem` asString "#.") $ l of
      (pat, [r]) -> Just (parseLine pat, r)
      _ -> Nothing
    parseLine = S.fromList . map fst . filter snd . zip finites
