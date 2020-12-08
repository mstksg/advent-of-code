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

module ParseT where

import Data.Symbol.Utils
import Data.Symbol.Ascii
import GHC.TypeLits
import Data.Proxy
import Data.Monoid

-- Generic tools

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or True _ = True
  Or _ True = True
  Or _ _ = False

type family (<%>) (a :: k) (b :: k) :: k

type instance (<%>) (xs :: Symbol) (ys :: Symbol) = AppendSymbol xs ys
type instance (<%>) (xs :: [a]) (ys :: [a]) = AppendList xs ys

type family Mempty :: k

type instance Mempty = ""
type instance Mempty = '[]

type family AppendList (xs :: [a]) (ys :: [a]) :: [a] where
  AppendList '[] ys = ys
  AppendList (x ': xs) ys = x ': (AppendList xs ys)

type family CatSymbols (as :: [Symbol]) :: Symbol where
  CatSymbols '[] = ""
  CatSymbols (x ': xs) = x <%> CatSymbols xs

-- Parser type

data P a where
  PChar :: Symbol -> P Symbol
  PApp :: P a -> P a -> P a
  PTry :: P a -> P a
  PAlt :: P a -> P a -> P a
  PLabel :: P a -> Symbol -> P a
  PNotChar :: Symbol -> P Symbol
  PSome :: P a -> P [a]
  PMany :: P a -> P [a]
  PPure :: a -> P a
  PAnyOf :: Symbol -> P Symbol
  PMap :: (a -> F b) -> P a -> P b

data F b where
  FList :: a -> F [a]
  FCat :: [a] -> F a

type family RunF (f :: F b) :: b where
  RunF (FList a) = '[a]
  RunF (FCat '[]) = Mempty
  RunF (FCat (x ': xs)) = x <%> RunF (FCat xs)

-- Parse result

data PO a = Success Bool a [Symbol] | Fail Bool Symbol

type family RunP (pa :: P a) (txt :: [Symbol]) :: PO a where
  RunP (PChar a) '[] = Fail False ("Expected " <%> a <%> " at EOF")
  RunP (PChar a) (a ': as) = Success True a as
  RunP (PChar a) as = Fail False ("Expected " <%> a <%> " at '" <%> CatSymbols as <%> "'")
  RunP (PApp f g) xs = RunApp (RunP f xs) g
  RunP (PTry f) xs = RunTry (RunP f xs)
  RunP (PAlt f g) xs = RunAlt (RunP f xs) g xs
  RunP (PLabel f label) xs = RunLabel (RunP f xs) label xs
  RunP (PNotChar a) '[] = Fail False ("Unexpected EOF")
  RunP (PNotChar a) (a ': as) = Fail False ("Unexpected " <%> a <%> " at '" <%> CatSymbols (a ': as) <%> "'")
  RunP (PNotChar a) (b ': as) = Success True b as
  RunP (PSome f) xs = RunP (PApp (PMap FList f) (PMany f)) xs
  RunP (PMany f) xs = RunP (PAlt (PSome f) (PPure '[])) xs
  RunP (PPure a) xs = Success False a xs
  RunP (PAnyOf cs) xs = RunLabel (RunAnyOf (ToList cs) xs) ("any of " <%> cs) xs
  RunP (PMap f g) xs = RunMap f (RunP g xs)

type family PText (txt :: Symbol) :: P Symbol where
  PText sym = PTry (PText' (ToList sym)) `PLabel` sym

type family PText' (txt :: [Symbol]) :: P Symbol where
  PText' '[] = PPure ""
  PText' (x ': xs) = PApp (PChar x) (PText' xs)

type family RunAnyOf (cs :: [Symbol]) (xs :: [Symbol]) where
  RunAnyOf (c ': cs) (c ': xs) = Success True c xs
  RunAnyOf (c ': cs) (x ': xs) = RunAnyOf cs (x ': xs)
  RunAnyOf (c ': cs) '[] = Fail False ""
  RunAnyOf '[]        xs = Fail False ""

type family RunMap (f :: a -> F b) (pg :: PO a) :: PO b where
  RunMap f (Success e a xs) = Success e (RunF (f a)) xs
  RunMap f (Fail e msg) = Fail e msg

type family RunApp (pf :: PO t) (g :: P t) :: PO t where
  RunApp (Success e a xs) g = RunApp' e a (RunP g xs)
  RunApp (Fail e msg) g = Fail e msg

type family RunApp' (e :: Bool) (a :: t) (pg :: PO t) where
  RunApp' e a (Success e2 a2 xs) = Success (Or e e2) (a <%> a2) xs
  RunApp' e a (Fail e2 msg) = Fail (Or e e2) msg

type family RunTry (pf :: PO a) :: PO a where
  RunTry (Success e a xs) = Success e a xs
  RunTry (Fail e msg) = Fail False msg

type family RunAlt (pf :: PO a) (g :: P a) (xs :: [Symbol]) :: PO a where
  RunAlt (Success e a xs) _ _ = Success e a xs
  RunAlt (Fail True msg) _ _ = Fail True msg
  RunAlt (Fail False msg) g xs = RunP g xs

type family RunLabel (pf :: PO a) (label :: Symbol) (xs :: [Symbol]) :: PO a where
  RunLabel (Success e a xs) _ _ = Success e a xs
  RunLabel (Fail e _) label xs = Fail e ("Expected " <%> label <%> " at '" <%> CatSymbols xs <%> "'")
