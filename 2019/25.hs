{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Monad
import qualified Criterion as Cr
import qualified Criterion.Main as Cr
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Traversable
import           Debug.Trace
import           System.IO
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators

import           AStar
import           IntCode
import           Util

prog :: IntCode
prog = unsafePerformIO $ makeProg . map read . splitOn "," . strip <$> readFile "input/25.txt"

attempt p = fmap Just (try p) <|> pure Nothing

parseResponse :: String -> Maybe (StateInfo -> StateInfo)
parseResponse txt = parseString (attempt parseResponseP) txt

parseResponseP :: Parser m => m (StateInfo -> StateInfo)
parseResponseP = move <|> pickup
  where
    move = do
      try $ spaces >> text "== "
      location <- T.dropWhileEnd isSpace . T.pack <$> some (noneOf ['='])
      text "=="
      spaces
      desc <- T.pack <$> manyTill anyChar (text "Doors here lead:\n")
      doors <- fmap (map T.pack) $ endBy1 (text "- " *> some letter) (char '\n')
      spaces
      msg <- optional (text "A loud, robotic voice says" >> some (noneOf ['\n']) >> spaces)
      items <- fmap (map T.pack) $ fmap fold $ optional $ do
        text "Items here:\n"
        endBy1 (text "- " *> some (noneOf ['\n'])) (char '\n')
      spaces
      (do text "Command?\n"
          return (\i -> i {stLocation = location,
                           stLocationDesc = desc,
                           stDoors = doors,
                           stFloor = items}))
        <|> move
    pickup = do
      spaces
      text "You take the "
      item <- T.pack <$> some (noneOf ['.'])
      text "."
      spaces
      text "Command?\n"
      return (\i -> i {stInv = Set.insert item (stInv i) })

send :: String -> Effect -> Effect
send [] eff = eff
send (a:as) (InputF k) = send as (k (ord a))

recv :: Effect -> (Effect, String)
recv (OutputF a k) = let ~(e, as) = recv k
                     in (e, chr a:as)
recv eff = (eff, [])

type State = (Effect, StateInfo)

data StateInfo = StateInfo {
  stLocation :: Text,
  stLocationDesc :: Text,
  stDoors :: [Text],
  stFloor :: [Text],
  stInv :: Set Text
  }
  deriving Show

main :: IO ()
main = do
  -- commands <- getContents
  -- let output = runProg prog (map ord commands)
  -- putStr $ map chr output

  let
    actions (eff, st@StateInfo{..}) = moves <|> pickup
      where
        moves = do
          door <- stDoors
          let (newProgram, resp) = recv $ send (T.unpack $ door <> "\n") eff
          case parseResponse resp of
            Just r -> return (newProgram, r st)
            Nothing -> error resp -- Crash and output the answer
        pickup = do
          item <- stFloor
          guard (not $ elem item ["infinite loop", "giant electromagnet"])
          let (newProgram, resp) = recv $ send ("take " <> T.unpack item <> "\n") eff
          case parseResponse resp of
            Just r -> return (newProgram, r st)
            Nothing -> mzero

    start =
      let (newProgram, resp) = recv $ (step prog) in
      case parseResponse resp of
        Just r -> (newProgram, r StateInfo{stInv = Set.empty})

  let states = exploreOn (\(_,i) -> (stLocation i, stInv i)) actions start :: [State]

  let rooms = map (stLocation . snd) (exploreOn (\(_,i) -> stLocation i) actions start :: [State])

  traverse_ print . map snd $ states

  -- let go eff info = do
  --       (eff, msg) <- pure $ recv eff
  --       putStr msg
  --       cmd <- getLine
  --       case parseResponse msg of
  --         Just r | T.pack cmd `elem` rooms -> do
  --                    let (neweff, newInfo) = bfsOn (stLocation . snd) actions (\st -> stLocation (snd st) == T.pack cmd) (eff, r info)
  --                    print newInfo
  --                    go neweff newInfo
  --         Just r -> do
  --           eff <- pure $ send (cmd ++ "\n") eff
  --           go eff (r info)
  --         _ -> do
  --           eff <- pure $ send (cmd ++ "\n") eff
  --           go eff info

  -- let (startProg, startInfo) = head [(eff, info) | (eff, info) <- states, stLocation info == "Security Checkpoint" && stInv info == Set.fromList ["astrolabe","hologram","klein bottle","tambourine"]]

  -- -- go (step prog) (StateInfo{stInv = Set.empty})
  -- go startProg startInfo
