-- |
-- Module      : AOC2016.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
module AOC2016.Day05 (
  day05a,
  day05b,
) where

import AOC.Common (foldMapParChunk, hexDigit, splitWord, _ListTup)
import AOC.Solver ((:~>) (..))
import Control.Lens (review, view)
import qualified Crypto.Hash as H
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.ByteString.Lens (packedChars)
import Data.Finite (Finite, strengthenN)
import Data.Foldable (toList)
import Data.List (find, scanl')
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)

coolHash :: H.Context H.MD5 -> Int -> Maybe (Finite 16, Finite 16)
coolHash ctx i = case concatMap (review _ListTup . splitWord) (BS.unpack hashed) of
  0 : 0 : 0 : 0 : 0 : x : y : _ -> Just (x, y)
  _ -> Nothing
  where
    hashed =
      BA.convert
        . H.hashFinalize
        . H.hashUpdate ctx
        . view (packedChars @BS.ByteString)
        $ show i

day05a :: H.Context H.MD5 :~> [Finite 16]
day05a =
  MkSol
    { sParse = Just . H.hashUpdate H.hashInit . view (packedChars @BS.ByteString)
    , sShow = map (review hexDigit)
    , sSolve = \ctx ->
        Just
          . take 8
          . (foldMap . foldMapParChunk 500_000)
            (maybeToList . fmap fst . coolHash ctx)
          $ chunksOf 10_000_000 [0 ..]
    }

coolHash2 :: H.Context H.MD5 -> Int -> Maybe (Finite 8, Finite 16)
coolHash2 ctx i = do
  (x, y) <- coolHash ctx i
  k <- strengthenN x
  pure (k, y)

day05b :: H.Context H.MD5 :~> Map (Finite 8) (Finite 16)
day05b =
  MkSol
    { sParse = Just . H.hashUpdate H.hashInit . view (packedChars @BS.ByteString)
    , sShow = map (review hexDigit) . toList
    , sSolve = \ctx ->
        find ((== 8) . M.size)
          . scanl' (\mp (k, x) -> M.insertWith (const id) k x mp) M.empty
          . (foldMap . foldMapParChunk 500_000)
            (maybeToList . coolHash2 ctx)
          $ chunksOf 10_000_000 [0 ..]
    }
