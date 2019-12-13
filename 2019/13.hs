{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Debug.Trace
import           Linear.V2
import           Linear.V3
import           Linear.V4

data IntCode = IntCode {
  memory :: IntMap Int,
  pc :: Int,
  relativeBase :: Int
  }
  deriving Show

data Effect = Input (Int -> Effect)
            | Output Int Effect
            | HaltEff

data Param = Imm Int | Pos Int | Rel Int
  deriving Show

data Op = Halt | Add | Mul | In | Out | JNZ | JZ | TestLT | TestEQ | AdjustBase
  deriving Show

(?) :: IntMap Int -> Int -> Int
mem ? i = IntMap.findWithDefault 0 i mem

decodeInstruction :: IntMap Int -> Int -> (Op, [Param])
decodeInstruction mem pc = case (mem ? pc) `mod` 100 of
  99 -> (Halt, [])
  1 -> (Add, arity 3)
  2 -> (Mul, arity 3)
  3 -> (In, arity 1)
  4 -> (Out, arity 1)
  5 -> (JNZ, arity 2)
  6 -> (JZ, arity 2)
  7 -> (TestLT, arity 3)
  8 -> (TestEQ, arity 3)
  9 -> (AdjustBase, arity 1)
  n -> error ("Unknown opcode: " ++ show n)
  where
    modes = [case c of {'0' -> Pos; '1' -> Imm; '2' -> Rel} | c <- reverse $ show ((mem ? pc) `div` 100)] ++ repeat Pos
    arity n = zipWith id modes [mem ? (pc + i) | i <- [1..n]]

step :: IntCode -> Effect
step m@IntCode{..} = case instruction of
  (Halt, []) -> HaltEff
  (Add, [a, b, c]) -> store c (ind a + ind b)
  (Mul, [a, b, c]) -> store c (ind a * ind b)
  (In,        [i]) -> Input (\val -> store i val)
  (Out,       [o]) -> Output (ind o) continue
  (JNZ, [b, t])
    | ind b /= 0       -> jump (ind t)
    | otherwise        -> continue
  (JZ, [b, t])
    | ind b == 0       -> jump (ind t)
    | otherwise        -> continue
  (TestLT, [a, b, t]) -> store t (if ind a < ind b then 1 else 0)
  (TestEQ, [a, b, t]) -> store t (if ind a == ind b then 1 else 0)
  (AdjustBase, [x]) -> step m{pc = pc + instructionSize,
                              relativeBase = relativeBase + ind x}
  _ -> error ("Bad instruction: " ++ show instruction)
  where
    ind (Imm x) = x
    ind (Pos x) = memory ? x
    ind (Rel x) = memory ? (x + relativeBase)
    loc (Pos x) = x
    loc (Rel x) = x + relativeBase
    instruction = decodeInstruction memory pc
    instructionSize = 1 + length (snd instruction)
    jump pc = step m{pc = pc}
    continue = step m{pc = pc + instructionSize}
    store i x = step m{pc = pc + instructionSize,
                       memory = IntMap.insert (loc i) x memory}

count x xs = sum (fmap (\y -> if y == x then 1 else 0) xs)

makeProg :: [Int] -> IntCode
makeProg xs = IntCode { memory = IntMap.fromList (zip [0..] xs),
                        pc = 0,
                        relativeBase = 0 }

prog = makeProg . map read . splitOn "," . fold . words <$> readFile "input/13.txt"

solve1 :: IntCode -> Int
solve1 prog = go (step prog) Map.empty
  where
    go HaltEff screen = count 2 screen
    go (Output x (Output y (Output t k))) screen
      = go k (Map.insert (V2 x y) t screen)

draw :: Map (V2 Int) Int -> Int -> IO ()
draw screen score = do
  putStrLn ("=== score: " ++ show score ++ " ===")
  let xmin = minimum (map (view _x) (Map.keys screen))
      ymin = minimum (map (view _y) (Map.keys screen))
      xmax = maximum (map (view _x) (Map.keys screen))
      ymax = maximum (map (view _y) (Map.keys screen))
      c 0 = " "
      c 1 = "#"
      c 2 = "\ESC[41m \ESC[m"
      c 3 = "\ESC[42m \ESC[m"
      c 4 = "\ESC[34mo\ESC[m"
  putStrLn $ unlines [
    fold [c (Map.findWithDefault 0 (V2 x y) screen) | x <- [xmin..xmax]]
    | y <- [ymin..ymax]]

solve2 :: IntCode -> IO ()
solve2 prog = go (step prog { memory = IntMap.insert 0 2 (memory prog)}) Map.empty 0
  where
    go HaltEff screen score = print score
    go (Output x (Output y (Output t k))) screen score
      | (x, y) == (-1, 0) = go k screen t
      | otherwise = go k (Map.insert (V2 x y) t screen) score
    go (Input k) screen score = do
      draw screen score
      let
        ball = head [p | (p, v) <- Map.toList screen, v == 4]
        paddle = head [p | (p, v) <- Map.toList screen, v == 3]
        joystick
          | view _x (ball - paddle) > 0 = 1
          | view _x (ball - paddle) < 0 = -1
          | view _x (ball - paddle) == 0 = 0
      go (k joystick) screen score
