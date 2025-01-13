-- |
-- Module      : AOC2024.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
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
module AOC2024.Day17
where

-- (
-- day17a,
-- day17b,
-- )

import AOC.Prelude
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import qualified Data.Conduino as C
import qualified Data.Conduino.Combinators as C
import Data.Finite hiding (shift)
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
import Data.Primitive.MutVar
import Data.STRef
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable.Sized as SMV
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Storable.Mutable.Sized as SMVS
import qualified Data.Vector.Storable.Sized as SVS
import qualified Linear as L
import qualified Numeric.Lens as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

day17a :: _ :~> [Int]
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
        pure (a, b, c, p)
    , sShow = intercalate "," . map show
    , sSolve = \(a, b, c, instrs) -> do
        pure . map fromIntegral $ stepProg instrs (V3 a b c)
    }

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

readCombo :: Combo -> V3 Word -> Word
readCombo = \case
  CLiteral l -> \_ -> fromIntegral l
  CReg r -> view (SV.fromTuple (_x, _y, _z) `SV.index` r)

stepProg :: SV.Vector 8 Instr -> V3 Word -> [Finite 8]
stepProg tp (V3 a0 b0 c0) = stepAll 0 a0 b0 c0
  where
    stepAll = stepWith tp (\o i a b c -> o : stepAll i a b c) stepAll

type Stepper a = Finite 8 -> Word -> Word -> Word -> a

stepWith ::
  Monoid a =>
  SV.Vector 8 Instr ->
  -- | out
  (Finite 8 -> Stepper a) ->
  -- | next
  Stepper a ->
  Stepper a
stepWith tp out next i !a !b !c = case tp `SV.index` i of
  ADV r -> withStep next (a `div` (2 ^ combo r)) b c
  BXL l -> withStep next a (b `xor` fromIntegral l) c
  BST r -> withStep next a (combo r `mod` 8) c
  JNZ l
    | a == 0 -> withStep next 0 b c
    | otherwise -> next (weakenN l) a b c
  BXC -> withStep next a (b `xor` c) c
  OUT r ->
    let o = modulo (fromIntegral (combo r))
     in withStep (out o) a b c
  BDV r -> withStep next a (a `div` (2 ^ combo r)) c
  CDV r -> withStep next a b (a `div` (2 ^ combo r))
  where
    combo = \case
      CLiteral l -> fromIntegral l
      CReg 0 -> a
      CReg 1 -> b
      CReg _ -> c
    withStep p
      | i == maxBound = \_ _ _ -> mempty
      | otherwise = p (i + 1)

-- -- | Assumes that:
-- --
-- -- 1. Only A is persistent across each "loop"
-- -- 2. The last instruction is a jump to 0
-- unstepProg :: SV.Vector 8 Instr -> [Finite 8] -> [Int]
-- unstepProg prog = unLoop jnzIx 0 Nothing Nothing
--   where
--     jnzIx :: Finite 8
--     jnzIx = maxBound
--     unLoop :: Finite 8 -> Word -> Maybe Word -> Maybe Word -> [Finite 8] -> [Int]
--     unLoop i a b c os = case prog `SV.index` i of
--       ADV r -> do
--         unshift <- fromIntegral <$> combo r
--         possibleUnshifts <- [0.. 2^unshift - 1]
--         withStep (a `shift` unshift + possibleUnshifts) b c os
--       BXL l -> do
--         b' <- maybeToList b
--         withStep a (Just $ b' `xor` fromIntegral l) c os
--       BST r -> case r of
--          CLiteral l -> do
--            for_ b \b' -> guard $ fromIntegral l == b'
--            withStep a b c os
--          CReg g -> do
--            possibleUnshift <- [0 .. 7]
--            let stored = fromMaybe 0 b `shift` 3 + possibleUnshift
--            case g of
--              0 -> withStep stored b c os
--              1 -> withStep a (Just stored) c os
--              _ -> withStep a b (Just stored) os
--       JNZ _
--         | a == 0    -> undefined
--         | otherwise -> undefined
--       BXC -> _
--         -- withStep a (b `xor` c) c
--       OUT r -> do
--         o:os' <- pure os
--         let setModulo x = ((x `shift` (-3)) `shift` 3) + fromIntegral o
--         case r of
--           CLiteral l -> do
--             guard $ weakenN l == o
--             withStep a b c os'
--           CReg 0 -> withStep (setModulo a) b c os'
--           CReg 1 -> withStep a (Just $ maybe (fromIntegral o) setModulo b) c os'
--           CReg _ -> withStep a b (Just $ maybe (fromIntegral o) setModulo c) os'
--       where
--         combo = \case
--           CLiteral l -> [fromIntegral l]
--           CReg 0 -> pure a
--           CReg 1 -> maybeToList b   -- hmm could really be anything
--           CReg _ -> maybeToList c   -- hmm could really be anything
--         withStep
--           | i == minBound = undefined
--           | otherwise = unLoop (pred i)

-- search 0
-- where
--   search a = \case
--     [] -> pure a
--     o : os -> do
--       a' <- ((a `shift` 3) +) <$> [0 .. 7]
--       let b0 = (a' .&. 7) `xor` 6
--       let c = a' `shift` (-b0)
--       guard $ modulo (fromIntegral $ (b0 `xor` c) `xor` 4) == o
--       search a' os

-- 2,4, 1,6, 7,5, 4,6, 1,4, 5,5, 0,3, 3,0
--
-- BST A    --- b = (a `mod` 8)
-- BXL 6    --- b ^= 110    (6)
-- CDV B    --- c = a / (2^b)
-- BXC      --- b ^= c
-- BXL 4    --- b ^= 100    (4)
-- OUT B    --- print b
-- ADV 3    --- a /= 8
-- JNZ 0

-- The *`adv`* instruction (opcode *`0`*) performs *division*. The
-- numerator is the value in the `A` register. The denominator is found by
-- raising 2 to the power of the instruction's *combo* operand. (So, an
-- operand of `2` would divide `A` by `4` (`2^2`); an operand of `5` would
-- divide `A` by `2^B`.) The result of the division operation is

-- * truncated* to an integer and then written to the `A` register.

-- The *`bxl`* instruction (opcode *`1`*) calculates the [bitwise
-- XOR](https://en.wikipedia.org/wiki/Bitwise_operation#XOR){target="_blank"}
-- of register `B` and the instruction's *literal* operand, then stores the
-- result in register `B`.

-- The *`bst`* instruction (opcode *`2`*) calculates the value of its

-- * combo* operand

-- [modulo](https://en.wikipedia.org/wiki/Modulo){target="_blank"} 8
-- (thereby keeping only its lowest 3 bits), then writes that value to the
-- `B` register.

-- The *`jnz`* instruction (opcode *`3`*) does *nothing* if the `A`
-- register is `0`. However, if the `A` register is *not zero*, it
-- [*jumps*]{title="The instruction does this using a little trampoline."}
-- by setting the instruction pointer to the value of its *literal*
-- operand; if this instruction jumps, the instruction pointer is *not*
-- increased by `2` after this instruction.

-- The *`bxc`* instruction (opcode *`4`*) calculates the *bitwise XOR* of
-- register `B` and register `C`, then stores the result in register `B`.
-- (For legacy reasons, this instruction reads an operand but *ignores*
-- it.)

-- The *`out`* instruction (opcode *`5`*) calculates the value of its

-- * combo* operand modulo 8, then *outputs* that value. (If a program

-- outputs multiple values, they are separated by commas.)

-- The *`bdv`* instruction (opcode *`6`*) works exactly like the `adv`
-- instruction except that the result is stored in the *`B` register*. (The
-- numerator is still read from the `A` register.)

-- The *`cdv`* instruction (opcode *`7`*) works exactly like the `adv`
-- instruction except that the result is stored in the *`C` register*. (The
-- numerator is still read from the `A` register.)
-- --

day17b :: _ :~> _
day17b =
  MkSol
    { sParse = parseMaybe' do
        _ <- "Register A: " *> pDecimal @Int
        P.newline
        _ <- "Register B: " *> pDecimal @Int
        P.newline
        _ <- "Register C: " *> pDecimal @Int
        P.newline
        P.newline
        "Program: " *> (pDecimal `sepBy'` ",")
    , sShow = show
    , sSolve =
        \p -> listToMaybe do
          option <- stepBackwards (reverse p)
          guard $ go 0 (V3 option 0 0) (Seq.fromList p) == p
          pure option
    }

go :: Int -> V3 Int -> Seq Int -> [Int]
-- go i (V3 a b c) tp = case (,) <$> Seq.lookup i tp <*> Seq.lookup (i + 1) tp of
go i (V3 a b c) tp = case (,) <$> Seq.lookup i tp <*> Seq.lookup (i + 1) tp of
  Nothing -> []
  Just (q, o) ->
    let x = case o of
          0 -> 0
          1 -> 1
          2 -> 2
          3 -> 3
          4 -> a
          5 -> b
          6 -> c
     in case q of
          0 -> go (i + 2) (V3 (a `div` (2 ^ x)) b c) tp
          1 -> go (i + 2) (V3 a (b `xor` o) c) tp
          2 -> go (i + 2) (V3 a (x `mod` 8) c) tp
          3
            | a == 0 -> go (i + 2) (V3 a b c) tp
            | otherwise -> go o (V3 a b c) tp
          4 -> go (i + 2) (V3 a (b `xor` c) c) tp
          5 -> (x `mod` 8) : go (i + 2) (V3 a b c) tp
          -- 5 -> trace (show (x `mod` 8, o, x)) (x `mod` 8) : go (i + 2) (V3 a b c) tp
          6 -> go (i + 2) (V3 a (a `div` (2 ^ x)) c) tp
          7 -> go (i + 2) (V3 a b (a `div` (2 ^ x))) tp

-- [ (go 0 (V3 i b c) (Seq.fromList p))
-- -- | i <- [45184372088832]
-- \| i <- [1999]
-- ]

stepBackwards :: [Int] -> [Int]
stepBackwards = search 0
  where
    search a = \case
      [] -> pure a
      o : os -> do
        a' <- ((a `shift` 3) +) <$> [0 .. 7]
        let b0 = (a' .&. 7) `xor` 6
        let c = a' `shift` (-b0)
        guard $ ((b0 `xor` c) `xor` 4) .&. 7 == o
        search a' os

-- traceM $ show $ V3 last10A last3B last7C
-- pure ()

-- CDV B    --- c = a / (2^b)
-- BXC      --- b ^= c
--
-- c = a / (2^b0)
-- b = b0 ^ c
--
-- b = b0 ^ (a >> b0)
--
-- b = (b0 ^ 110) ^ (a >> (b0 ^ 110))
--
-- b = ((a `mod` 8) ^ 110) ^ (a >> ((a `mod` 8) ^ 110))
--
-- mod 8 is & 111
--
-- b & 111 = ((a & 111) ^ 110) ^ (a >> ((a & 111) ^ 110))
-- b & 111 = ((a ^ 110) & 111) ^ (a >> ((a ^ 110) & 111))
--
-- i guess we can do a search for what (a & 111) is, it's only 7 possible
-- items. But how do we get from that to the full A?
--
-- Ah hah i guess we can store the rest of the A trit-by-trit.
--
--
--

-- stepBackwards r@(V3 a b c) = \case
--   [] -> r
--   x:xs ->

-- BST A    --- b = (a `mod` 8)
-- BXL 6    --- b ^= 110    (6)
-- CDV B    --- c = a >> b
-- BXC      --- b ^= c
-- BXL 4    --- b ^= 100    (4)
-- OUT B    --- print (b `mod` 8)
-- ADV 3    --- a /= 8
-- JNZ 0
