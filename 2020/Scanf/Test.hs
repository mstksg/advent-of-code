{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Scanf.Plugin #-}

module Scanf.Test where

import           Control.Applicative
import           Control.Monad
import           Data.Kind
import           Data.Singletons
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Typeable as Ty
import           GHC.TypeLits
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)
import           Text.Parser.LookAhead
import           Text.Trifecta (Parser)
import qualified Text.Trifecta as Trifecta

import           Scanf.Scanf
import           Scanf.Specifier

parseString :: Parser a -> String -> a
parseString p txt = case Trifecta.parseString p mempty txt of
  Trifecta.Success a -> a
  Trifecta.Failure e -> error (show (Trifecta._errDoc e))

abc _ = scanf @"abc %r"
