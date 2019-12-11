{-# LANGUAGE RecordWildCards #-}

import           Control.Monad
import           Control.Lens
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.List
import           Linear.V2

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

makeProg :: [Int] -> IntCode
makeProg xs = IntCode { memory = IntMap.fromList (zip [0..] xs),
                        pc = 0,
                        relativeBase = 0 }

prog = makeProg [3,8,1005,8,326,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,29,2,1003,17,10,1006,0,22,2,106,5,10,1006,0,87,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,65,2,7,20,10,2,9,17,10,2,6,16,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,99,1006,0,69,1006,0,40,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,127,1006,0,51,2,102,17,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,155,1006,0,42,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,180,1,106,4,10,2,1103,0,10,1006,0,14,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,213,1,1009,0,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,239,1006,0,5,2,108,5,10,2,1104,7,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,272,2,1104,12,10,1,1109,10,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,302,1006,0,35,101,1,9,9,1007,9,1095,10,1005,10,15,99,109,648,104,0,104,1,21102,937268449940,1,1,21102,1,343,0,1105,1,447,21101,387365315480,0,1,21102,1,354,0,1105,1,447,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,29220891795,1,21102,1,401,0,1106,0,447,21101,0,248075283623,1,21102,412,1,0,1105,1,447,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,984353760012,1,21102,1,435,0,1105,1,447,21102,1,718078227200,1,21102,1,446,0,1105,1,447,99,109,2,21202,-1,1,1,21102,40,1,2,21101,0,478,3,21101,468,0,0,1106,0,511,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,473,474,489,4,0,1001,473,1,473,108,4,473,10,1006,10,505,1102,1,0,473,109,-2,2105,1,0,0,109,4,1202,-1,1,510,1207,-3,0,10,1006,10,528,21102,1,0,-3,22102,1,-3,1,22101,0,-2,2,21101,0,1,3,21102,1,547,0,1105,1,552,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,575,2207,-4,-2,10,1006,10,575,21202,-4,1,-4,1105,1,643,21202,-4,1,1,21201,-3,-1,2,21202,-2,2,3,21102,1,594,0,1106,0,552,22102,1,1,-4,21101,1,0,-1,2207,-4,-2,10,1006,10,613,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,635,22101,0,-1,1,21101,0,635,0,106,0,510,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0]

solve1 :: IntCode -> Int
solve1 prog = go (step prog) (Map.empty :: Map (V2 Int) Int) (V2 0 0) (V2 0 1)
  where
    go HaltEff panels pos face = Map.size panels
    go (Input k) panels pos face = go (k (Map.findWithDefault 0 pos panels)) panels pos face
    go (Output c (Output t k)) panels pos face = go k panels' pos' face'
      where
        panels' = Map.insert pos c panels
        face' = case t of
          0 -> perp face -- turn left
          1 -> -(perp face) -- turn right
        pos' = pos + face'

solve2 :: IntCode -> IO ()
solve2 prog = go (step prog) (Map.singleton (V2 0 0) 1) (V2 0 0) (V2 0 1)
  where
    go HaltEff panels pos face = draw panels
    go (Input k) panels pos face = go (k (Map.findWithDefault 0 pos panels)) panels pos face
    go (Output c (Output t k)) panels pos face = go k panels' pos' face'
      where
        panels' = Map.insert pos c panels
        face' = case t of
          0 -> perp face -- turn left
          1 -> -(perp face) -- turn right
        pos' = pos + face'

draw :: Map (V2 Int) Int -> IO ()
draw panels = putStrLn $ unlines $ do
  y <- reverse [ymin..ymax]
  return [case Map.findWithDefault 0 (V2 x y) panels of
            1 -> '#'
            0 -> '.'
            | x <- [xmin..xmax]]
  where
    xmin = minimum (map (view _x) (Map.keys panels))
    xmax = maximum (map (view _x) (Map.keys panels))
    ymin = minimum (map (view _y) (Map.keys panels))
    ymax = maximum (map (view _y) (Map.keys panels))

-- run :: IntCode -> [Int] -> [Int]
-- run mem ins = go (step mem) ins
--   where
--     go (Input f) ins = go (f (head ins)) (tail ins)
--     go (Output o f) ins = o : go f ins
--     go HaltEff ins = []

-- main :: IO ()
-- main = do
--   print (run prog [1])
--   print (run prog [2])
