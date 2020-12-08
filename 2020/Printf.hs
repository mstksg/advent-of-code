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

module Printf where

import Data.Symbol.Utils
import GHC.TypeLits
import Data.Proxy
import Data.Monoid

-- test :: String
-- test = printf @"Wurble %d %d %s" 10 20 "foo"

data Specifier
  = Spec Symbol
  | Lit Symbol

class FormatF (format :: [Specifier]) fun where
  formatF :: String -> fun

instance (a ~ String) => FormatF '[] a where
  formatF = id

instance (FormatF rest fun, a ~ (Int -> fun)) => FormatF (Spec "%d" ': rest) a where
  formatF str i
    = formatF @rest (str Data.Monoid.<> show i)

instance (FormatF rest fun, a ~ (String -> fun)) => FormatF (Spec "%s" ': rest) a where
  formatF str s
    = formatF @rest (str <> s)

instance (FormatF rest fun, f ~ (a -> fun), Show a) => FormatF (Spec "%r" ': rest) f where
  formatF str a
    = formatF @rest (str <> show a)

instance (FormatF rest fun, KnownSymbol l) => FormatF (Lit l ': rest) fun where
  formatF str
    = formatF @rest (str <> symbolVal (Proxy @l))

data P a where
  PChar :: Symbol -> P Symbol
  PApp :: P Symbol -> P Symbol -> P Symbol
  PTry :: P a -> P a
  PLabel :: P a -> Symbol -> P a
  PAlt :: P a -> P a -> P a

data PO a = Success Bool a [Symbol] | Fail Bool Symbol

type (<%>) a b = AppendSymbol a b

type family CatSymbols (as :: [Symbol]) :: Symbol where
  CatSymbols '[] = ""
  CatSymbols (x ': xs) = x <%> CatSymbols xs

type family RunApp (pf :: PO Symbol) (g :: P Symbol) :: PO Symbol where
  RunApp (Success e a xs) g = RunApp' e a (RunP g xs)
  RunApp (Fail e msg) g = Fail e msg

type family RunApp' (e :: Bool) (a :: Symbol) (pg :: PO Symbol) where
  RunApp' e a (Success e2 a2 xs) = Success (Or e e2) (a <%> a2) xs
  RunApp' e a (Fail e2 msg) = Fail (Or e e2) msg

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or True _ = True
  Or _ True = True
  Or _ _ = False

type family RunTry (pf :: PO a) :: PO a where
  RunTry (Success e a xs) = Success e a xs
  RunTry (Fail e msg) = Fail False msg

type family RunAlt (pf :: PO a) (g :: P a) (xs :: [Symbol]) :: PO a where
  RunAlt (Success e a xs) _ _ = Success e a xs
  RunAlt (Fail True msg) _ _ = Fail True msg
  RunAlt (Fail False msg) g xs = RunP g xs

type family RunLabel (pf :: PO a) (label :: Symbol) (xs :: [Symbol]) :: PO a where
  RunLabel (Success e a xs) _ _ = Success e a xs
  RunLabel (Fail e _) label xs = Fail e ("Expected " <%> label <%> " at " <%> CatSymbols xs)

type family RunP (pa :: P a) (txt :: [Symbol]) :: PO a where
  RunP (PChar a) '[] = Fail False ("Expected " <%> a <%> " at EOF")
  RunP (PChar a) (a ': as) = Success True a as
  RunP (PChar a) as = Fail False ("Expected " <%> a <%> " at " <%> CatSymbols as)
  RunP (PApp f g) xs = RunApp (RunP f xs) g
  RunP (PTry f) xs = RunTry (RunP f xs)
  RunP (PAlt f g) xs = RunAlt (RunP f xs) g xs
  RunP (PLabel f label) xs = RunLabel (RunP f xs) label xs


class PrintF (sym :: Symbol) fun where
  printf :: fun

type family Parse (lst :: [Symbol]) :: [Specifier] where
  Parse '[] = '[Lit ""]
  Parse ("%" ': x ': xs) = Spec (AppendSymbol "%" x) ': Parse xs
  Parse (x ': xs) = Parse2 x (Parse xs)

type family Parse2 (c :: Symbol) (lst :: [Specifier]) :: [Specifier] where
  Parse2 c ('Lit s ': ss) = 'Lit (AppendSymbol c s) ': ss
  Parse2 c ss = 'Lit c ': ss

instance (Listify sym lst, fmt ~ Parse lst, FormatF fmt fun) => PrintF (sym :: Symbol) fun where
  printf = formatF @fmt ""
