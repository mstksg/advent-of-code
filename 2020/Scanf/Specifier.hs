{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Scanf.Specifier where

import GHC.TypeLits (Symbol)

data Spec a = Lit a | SpecRead | SpecSpace | SpecRegex a

type SpecType = Spec Symbol

type family ParseScanf (fmt :: Symbol) :: [SpecType] where
