#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.text pkgs.template pkgs.optparse-applicative ])"

-- #! /usr/bin/env nix-shell
-- #! nix-shell -i bash -p bash
-- #!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-16 --package template --package text --package filepath --package directory -- -Wall
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Template
import qualified Options.Applicative as O
import System.Directory
import System.FilePath
import Text.Printf

data Opts = Opts {oYear :: Integer, oForce :: Bool}

parseOpts :: O.Parser Opts
parseOpts =
  Opts
    <$> O.argument
      O.auto
      (O.metavar "YEAR" <> O.help "Year to generate")
    <*> O.switch
      (O.long "force" <> O.short 'f' <> O.help "Overwrite files if present")

outRoot :: Integer -> FilePath
outRoot yr = show yr <> "/src/AOC" <> show yr

main :: IO ()
main = do
  Opts {..} <-
    O.execParser $
      O.info
        (parseOpts <**> O.helper)
        (O.fullDesc <> O.progDesc "Generate haskell daily challenge files" <> O.header "generate_days")
  let y = 2023
  temp <- template <$> T.readFile "template/DayXX.hs"
  forM_ [1 .. 25] $ \i -> do
    let newFilePath = outRoot y </> printf "Day%02d.hs" i
        Just newFile = renderA temp (ctx y i)
    alreadyExists <- doesFileExist newFilePath
    skip <-
      if alreadyExists
        then
          if oForce
            then False <$ printf "%s exists, but overwriting: --force supplied.\n" newFilePath
            else True <$ printf "%s exists, skipping (use --force to overwrite)\n" newFilePath
        else pure False
    unless skip $
      TL.writeFile newFilePath newFile

ctx :: Integer -> Int -> ContextA Maybe
ctx y i = \case
  "day" -> Just . T.pack $ printf "%02d" i
  "day_short" -> Just . T.pack $ printf "%d" i
  "year" -> Just . T.pack $ printf "%04d" y
  _ -> Nothing
