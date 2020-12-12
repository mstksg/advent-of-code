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

module TScanf where

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

import           Util
import qualified Util.Text as T

import           RegexPCRE (regex)
import           ScanfParser

data HList (l::[*]) where
    HNil :: HList '[]
    (:<) :: e -> HList l -> HList (e ': l)

instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
  showsPrec n (a :< as) = showsPrec 10 a . showString " :< " . showsPrec 10 as
instance Show (HList '[]) where
  showsPrec n HNil = showString "HNil"

class Scanf (specs :: [Specifier]) (as :: [Type]) where
  scanf' :: Parser (HList as)

instance (as ~ '[]) => Scanf '[] as where
  scanf' = pure HNil

instance (Scanf specs as, KnownSymbol txt) => Scanf (Lit txt ': specs) as where
  scanf' = do string (symbolVal (Proxy @ txt))
              scanf' @ specs

instance (Scanf specs as, Read a, Ty.Typeable a, aas ~ (a ': as)) => Scanf (SpecRead ': specs) aas where
  scanf' = do a <- parse_reads <?> show (Ty.typeOf (undefined :: a))
              as <- scanf' @ specs
              return (a :< as)

instance (Scanf specs as, KnownSymbol pat, aas ~ (Text ': as)) => Scanf (SpecRegex pat ': specs) aas where
  scanf' = do a <- regex (T.pack (symbolVal (Proxy @pat)))
              as <- scanf' @ specs
              return (a :< as)

scanf :: forall (pat :: Symbol) as. Scanf (ParseScanf pat) as => Parser (HList as)
scanf = scanf' @(ParseScanf pat)

-- This is pretty bad, it does infinite lookahead
parse_reads :: forall a. Read a => Parser a
parse_reads = do str <- lookAhead (some anyChar)
                 case reads str of
                   [(a :: a, leftover)] -> let n = length str - length leftover
                                      in read <$> string (take n str)
                   _ -> mzero

-- λ> parse (scanf @"hello, %r") "hello, 50" :: HList '[Int]
-- 50 :< HNil

-- λ> parse (scanf @"hello, %r") "hello, 50" :: HList '[Integer]
-- 50 :< HNil

-- λ> parse (scanf @"hello, %r") "hello, i'm not a number" :: HList '[Int]
-- *** Exception: (interactive):1:8: error: expected: Int
-- 1 | hello, i'm not a number<EOF>

-- -- Type error when the format string is unparseable:
-- λ> parse (scanf @"hello, %`") "hello, i'm not a number" :: HList '[Int]
-- <interactive>:72:8: error:
--     • expected eof at '%`'
--     • In the first argument of ‘parse’, namely ‘(scanf @"hello, %`")’

-- λ> parse (scanf @"hello, %/[a-z]+/s here") "hello, word here"
-- "word" :< HNil

-- λ> :t scanf @"%/[a-z]+/s = %r"
-- scanf @"%/[a-z]+/s = %r"
--   :: (Read a, Ty.Typeable a) => Parser (HList '[Text, a])
