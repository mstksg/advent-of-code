{-# LANGUAGE AllowAmbiguousTypes #-}
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

module Scanf.Scanf where

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

import           Scanf.Specifier

data HList (l::[*]) where
    HNil :: HList '[]
    (:<) :: e -> HList l -> HList (e ': l)
infixr 7 :<

instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
  showsPrec n (a :< as) = showsPrec 10 a . showString " :< " . showsPrec 10 as
instance Show (HList '[]) where
  showsPrec n HNil = showString "HNil"

class Scanf (specs :: [SpecType]) (as :: [Type]) where
  scanf' :: Parser (HList as)

instance (as ~ '[]) => Scanf '[] as where
  scanf' = pure HNil

instance (Scanf specs as, KnownSymbol txt) => Scanf (Lit txt ': specs) as where
  scanf' = do string (symbolVal (Proxy @ txt))
              scanf' @ specs

instance (Scanf specs as) => Scanf (SpecSpace ': specs) as where
  scanf' = do some (oneOf " ")
              scanf' @ specs

instance (Scanf specs as, Read a, Ty.Typeable a, aas ~ (a ': as)) => Scanf (SpecRead ': specs) aas where
  scanf' = do a <- parse_reads <?> show (Ty.typeOf (undefined :: a))
              as <- scanf' @ specs
              return (a :< as)

-- instance (Scanf specs as, KnownSymbol pat, aas ~ (Text ': as)) => Scanf (SpecRegex pat ': specs) aas where
--   scanf' = do a <- regex (T.pack (symbolVal (Proxy @pat)))
--               as <- scanf' @ specs
--               return (a :< as)

-- This is pretty bad, it does infinite lookahead
parse_reads :: forall a. Read a => Parser a
parse_reads = do str <- lookAhead (some anyChar)
                 case reads str of
                   [(a :: a, leftover)] -> let n = length str - length leftover
                                      in read <$> string (take n str)
                   _ -> mzero

-- scanf :: forall fmt as. (Scanf (ParseScanf fmt) as) => Parser (HList as)
-- scanf = scanf' @(ParseScanf fmt)

scanf :: forall fmt as. (Scanf (ParseScanf fmt) as) => Parser (HList as)
scanf = scanf' @(ParseScanf fmt)
