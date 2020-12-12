{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ScanfParser where

import Data.Singletons
import GHC.TypeLits (Symbol)

import SymbolParser

data Specifier = Lit Symbol | SpecRead | SpecRegex Symbol

type PScanf = PSome ((TyCon1 Lit <$> SomeText (NoneOf "%" <|>
                                               ("%" $> (PText "%%")))) <|>
                     (TyCon1 SpecRegex <$> (PText "%/" *> SomeText (NoneOf "/") <* PText "/s")) <|>
                     (SpecRead $> PText "%r")) <* PEOF

type ParseScanf pat = ExecParser PScanf pat
