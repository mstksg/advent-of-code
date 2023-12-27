{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Hakyll

config :: Configuration
config =
  defaultConfiguration
    { providerDirectory = "pages"
    }

main :: IO ()
main = hakyllWith config do
  match "reflections/*/*" do
    route $ setExtension "html"
    compile pandocCompiler
