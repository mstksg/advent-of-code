{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : AOC2019.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
module AOC2019.Day17 (
  day17a,
  day17b,
) where

import AOC.Common.Point (Dir (..), Point, cardinalNeighbs, dirPoint', parseAsciiMap)
import AOC.Solver ((:~>) (..))
import AOC.Util (eitherToMaybe)
import AOC2019.Common.Intcode (IErr, Memory, mRegLens, parseMem, stepForever)
import Control.Applicative (empty)
import Control.DeepSeq (NFData)
import Control.Lens (set)
import Control.Monad (guard, (<=<))
import Data.Char (chr, ord)
import Data.Conduino (feedPipe)
import Data.Foldable (asum)
import Data.List (group, inits, intercalate, stripPrefix, unfoldr)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Safe (lastMay)

data AState = AS
  { asPos :: Point
  , asDir :: Dir
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

parseMap :: Memory -> Maybe (Set Point, Maybe AState)
parseMap m = do
  (os, _) <- eitherToMaybe $ feedPipe [] (stepForever @IErr m)
  let mp = parseAsciiMap parseTile (map chr os)
      scaff = M.keysSet mp
      sOut = do
        (p, d) <- listToMaybe . M.toList . M.mapMaybe id $ mp
        pure (AS p d)
  pure (scaff, sOut)
  where
    parseTile = \case
      '#' -> Just Nothing
      '^' -> Just (Just North)
      '>' -> Just (Just East)
      'v' -> Just (Just South)
      '<' -> Just (Just West)
      _ -> Nothing

day17a :: Set Point :~> Int
day17a =
  MkSol
    { sParse = fmap fst . parseMap <=< parseMem
    , sShow = show
    , sSolve = Just . sum . S.map product . findNeighbs
    }
  where
    findNeighbs scaff = S.filter allScaff scaff
      where
        allScaff = all (`S.member` scaff) . cardinalNeighbs

day17b :: (Set Point, AState, Memory) :~> (String, Memory)
day17b =
  MkSol
    { sParse = \str -> do
        m <- set (mRegLens 0) 2 <$> parseMem str
        (scaff, as0) <- sequenceA =<< parseMap m
        pure (scaff, as0, m)
    , sShow = \(map ord -> inp, m) -> foldMap show $ do
        output <- fst <$> eitherToMaybe (feedPipe inp (stepForever @IErr m))
        lastMay output
    , sSolve = \(scaff, as0, m) -> do
        let path = findPath scaff as0
        (a, b, c) <- findProgs path
        let mainProg = chomp [(a, "A"), (b, "B"), (c, "C")] path
            inp =
              unlines . map (intercalate ",") $
                [ mainProg
                , showPC <$> a
                , showPC <$> b
                , showPC <$> c
                , ["n"]
                ]
        pure (inp, m)
    }

findProgs :: Eq a => [a] -> Maybe ([a], [a], [a])
findProgs p0 = listToMaybe $ do
  a <- validPrefix p0

  let withoutA = neSplitOn a p0
  b <- case withoutA of
    [] -> empty
    bs : _ -> validPrefix bs

  let withoutB = neSplitOn b =<< withoutA
  c <- case withoutB of
    [] -> empty
    cs : _ -> validPrefix cs

  let withoutC = neSplitOn c =<< withoutB
  guard $ null withoutC

  pure (a, b, c)
  where
    validPrefix = take 4 . filter (not . null) . inits
    neSplitOn x = filter (not . null) . splitOn x

chomp :: Eq a => [([a], b)] -> [a] -> [b]
chomp progs = unfoldr go
  where
    go xs =
      asum
        [ (r,) <$> stripPrefix prog xs
        | (prog, r) <- progs
        ]

type PathComp = Either Int Int

showPC :: PathComp -> String
showPC = \case
  Left x -> "L," ++ show x
  Right x -> "R," ++ show x

findPath :: Set Point -> AState -> [PathComp]
findPath scaff = mapMaybe process . chunksOf 2 . group . unfoldr go
  where
    process = \case
      [Just turnRight : _, steps]
        | turnRight -> Just $ Right (length steps)
        | otherwise -> Just $ Left (length steps)
      _ -> Nothing
    go AS{..}
      | forward `S.member` scaff = Just (Nothing, AS forward asDir)
      | turnLeft `S.member` scaff = Just (Just False, AS asPos (asDir <> West))
      | turnRight `S.member` scaff = Just (Just True, AS asPos (asDir <> East))
      | otherwise = Nothing
      where
        forward = asPos + dirPoint' asDir
        turnLeft = asPos + dirPoint' (asDir <> West)
        turnRight = asPos + dirPoint' (asDir <> East)
