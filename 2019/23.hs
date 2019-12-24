{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Lens
import           Control.Monad
import qualified Criterion as Cr
import qualified Criterion.Main as Cr
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Traversable
import           Debug.Trace
import           System.IO.Unsafe

import           AStar
import           IntCode
import           Util

prog :: IntCode
prog = unsafePerformIO $ makeProg . map read . splitOn "," . strip <$> readFile "input/23.txt"

runMachine :: IntCode -> [TVar Bool] -> [TQueue (Int, Int)] -> Int -> IO ()
runMachine prog idle queues n = start (step prog)
  where
    start (InputF k) = go (k n)
    go HaltF = pure()
    go (OutputF i (OutputF x (OutputF y k))) = do
      atomically $ do writeTQueue (queues !! i) (x,y)
                      writeTVar (idle !! i) False
                      writeTVar (idle !! n) False
      -- print (i, x, y)
      go k
    go (InputF k) = do
      may <- atomically $ do
        isIdle <- readTVar (idle !! n)
        may <- (if isIdle then fmap Just . readTQueue else tryReadTQueue) (queues !! n)
        case may of
          Just _ -> return may
          Nothing -> do
            writeTVar (idle !! n) True
            return may
      case may of
        Just (x, y) -> case k x of
          InputF k -> go (k y)
        Nothing -> go (k (-1))

solve1 :: IO ()
solve1 = do
  queues <- replicateM 256 newTQueueIO
  idle <- replicateM 256 (newTVarIO False)
  ts <- for [0..49] $ \n -> do
    forkIO (runMachine prog idle queues n)
  atomically (readTQueue (queues !!255)) >>= print . snd
  traverse_ killThread ts

solve2 :: IO ()
solve2 = do
  queues <- replicateM 256 newTQueueIO
  idle <- replicateM 256 (newTVarIO False)
  ts <- for [0..49] $ \n -> do
    forkIO (runMachine prog idle queues n)
  let nat msg = do
        msg' <- atomically $ do
          isIdle <- and <$> traverse readTVar (take 50 idle)
          guard isIdle
          more <- flushTQueue (queues !! 255)
          let msg' = case more of
                [] -> msg
                _ -> last more
          writeTQueue (queues !! 0) msg'
          writeTVar (idle !! 0) False
          return msg'
        if snd msg' == snd msg then
          return (snd msg')
          else
          nat msg'

  finalY <- nat (0,0)
  print finalY
  traverse_ killThread ts

main = do
  solve1
  solve2
