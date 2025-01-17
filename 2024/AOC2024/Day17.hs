-- |
-- Module      : AOC2024.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
module AOC2024.Day17 (
  day17a,
  day17b,
)
where

import AOC.Common.Parser (pDecimal, parseMaybe', sepBy')
import AOC.Solver (noFail, type (:~>) (..))
import Control.Monad (guard)
import Data.Bits (Bits (shift, xor))
import Data.Finite (
  Finite,
  combineProduct,
  getFinite,
  modulo,
  packFinite,
  separateProduct,
  separateSum,
  strengthen,
  weakenN,
 )
import Data.Foldable (Foldable (toList))
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Monoid (Alt (..))
import Data.Semigroup (Endo (Endo, appEndo))
import qualified Data.Vector.Sized as SV
import qualified Text.Megaparsec.Char as P

data Combo
  = CLiteral (Finite 4)
  | CReg (Finite 3)
  deriving stock (Show, Eq, Ord)

data Instr
  = ADV Combo
  | BXL (Finite 8)
  | BST Combo
  | JNZ (Finite 4)
  | BXC
  | OUT Combo
  | BDV Combo
  | CDV Combo
  deriving stock (Show, Eq, Ord)

comboParser :: Finite 7 -> Combo
comboParser = either CLiteral CReg . separateSum

instrParser :: Finite 8 -> Finite 8 -> Maybe Instr
instrParser i =
  SV.fromTuple @_ @8
    ( fmap (ADV . comboParser) . strengthen
    , Just . BXL
    , fmap (BST . comboParser) . strengthen
    , Just . JNZ . snd . separateProduct @2 @4
    , const $ Just BXC
    , fmap (OUT . comboParser) . strengthen
    , fmap (BDV . comboParser) . strengthen
    , fmap (CDV . comboParser) . strengthen
    )
    `SV.index` i

parseProgram :: [Int] -> Maybe (SV.Vector 8 Instr)
parseProgram xs = do
  xsVec <- SV.fromList @16 =<< traverse (packFinite . fromIntegral) xs
  SV.generateM \i ->
    instrParser (xsVec `SV.index` combineProduct (0, i)) (xsVec `SV.index` combineProduct (1, i))

day17a :: (Word, Word, Word, SV.Vector 8 Instr, [Finite 8]) :~> [Finite 8]
day17a =
  MkSol
    { sParse = parseMaybe' do
        a <- "Register A: " *> pDecimal <* P.newline
        b <- "Register B: " *> pDecimal <* P.newline
        c <- "Register C: " *> pDecimal <* P.newline
        P.newline
        d <- "Program: " *> (pDecimal `sepBy'` ",")
        p <- case parseProgram d of
          Nothing -> fail "Bad program"
          Just p -> pure p
        pure (a, b, c, p, fromIntegral <$> d)
    , sShow = intercalate "," . map (show . getFinite)
    , sSolve = noFail \(a0, b0, c0, instrs, _) ->
        appEndo (stepWith instrs (Endo . (:)) 0 a0 b0 c0) []
    }

stepWith ::
  Monoid a =>
  SV.Vector 8 Instr ->
  -- | out
  (Finite 8 -> a) ->
  Finite 8 ->
  Word ->
  Word ->
  Word ->
  a
stepWith tp out = go
  where
    go i !a !b !c = case tp `SV.index` i of
      ADV r -> withStep go (a `div` (2 ^ combo r)) b c
      BXL l -> withStep go a (b `xor` fromIntegral l) c
      BST r -> withStep go a (combo r `mod` 8) c
      JNZ l
        | a == 0 -> withStep go 0 b c
        | otherwise -> go (weakenN l) a b c
      BXC -> withStep go a (b `xor` c) c
      OUT r ->
        let o = modulo (fromIntegral (combo r))
         in out o <> withStep go a b c
      BDV r -> withStep go a (a `div` (2 ^ combo r)) c
      CDV r -> withStep go a b (a `div` (2 ^ combo r))
      where
        combo = \case
          CLiteral l -> fromIntegral l
          CReg 0 -> a
          CReg 1 -> b
          CReg _ -> c
        withStep p
          | i == maxBound = \_ _ _ -> mempty
          | otherwise = p (i + 1)

searchStep :: SV.Vector 8 Instr -> [Finite 8] -> [Word]
searchStep tp outs = do
  JNZ 0 <- pure $ tp `SV.index` maxBound
  [CReg _] <- pure [r | OUT r <- toList tp]
  let search a = \case
        o : os -> do
          a' <- stepBack a
          guard $ stepForward a' == Just o
          search a' os
        [] -> pure a
  search 0 (reverse outs)
  where
    stepForward :: Word -> Maybe (Finite 8)
    stepForward a0 = getAlt $ stepWith tp (Alt . Just) 0 a0 0 0
    stepBack :: Word -> [Word]
    stepBack = go' maxBound
      where
        go' i a = case tp `SV.index` i of
          ADV r -> do
            a' <- case r of
              CLiteral l -> ((a `shift` fromIntegral l) +) <$> [0 .. 2 ^ getFinite l - 1]
              CReg _ -> []
            go' (pred i) a'
          OUT _ -> pure a
          _ -> go' (pred i) a

day17b :: (Word, Word, Word, SV.Vector 8 Instr, [Finite 8]) :~> Word
day17b =
  MkSol
    { sParse = sParse day17a
    , sShow = show
    , sSolve = \(_, _, _, instrs, o) -> listToMaybe $ searchStep instrs o
    }
