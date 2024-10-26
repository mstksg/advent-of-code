-- |
-- Module      : AOC2020.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!
module AOC2020.Day15 (
  day15a,
  day15b,
)
where

import AOC.Solver ((:~>) (..))
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (runST)
import Control.Monad.State.Strict (evalStateT, get, gets, put)
import Data.Foldable (for_)
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Int (Int32)
import Text.Read (readMaybe)

day15a :: [Int] :~> Int
day15a =
  MkSol
    { sParse = traverse readMaybe . splitOn ","
    , sShow = show
    , sSolve = Just . looper 2020
    }

day15b :: [Int] :~> Int
day15b =
  MkSol
    { sParse = sParse day15a
    , sShow = show
    , sSolve = Just . looper 30000000
    }

data LoopState = LS
  { lsLastSaid :: !Int
  , lsCurrTime :: !Int32
  }

looper :: Int -> [Int] -> Int
looper n xs0 = runST $ flip evalStateT (LS 0 0) $ do
  v <- MV.replicate n 0
  for_ xs0 $ \y -> do
    LS x i <- get
    MV.unsafeWrite v x i
    put (LS y (i + 1))
  whileM_ (gets ((< n32) . lsCurrTime)) $ do
    LS x i <- get
    lst <- MV.unsafeRead v x
    MV.unsafeWrite v x i
    let j
          | lst == 0 = 0
          | otherwise = i - lst
    put (LS (fromIntegral j) (i + 1))
  gets lsLastSaid
  where
    n32 :: Int32
    n32 = fromIntegral n
