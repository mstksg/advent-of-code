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

import Data.Singletons
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Bool
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Function
import Data.Singletons.Prelude.Monoid
import Data.Kind
import Data.Monoid
import Data.Proxy
import Data.Symbol.Ascii
import Data.Symbol.Utils
import GHC.TypeLits (Symbol, AppendSymbol)

-- Generic tools

type family (##) (a :: k) (b :: k) :: k

type instance (##) (xs :: Symbol) (ys :: Symbol) = Mappend xs ys
type instance (##) (xs :: [a]) (ys :: [a]) = Mappend xs ys

data SFold :: [a] ~> a
type instance Apply SFold '[] = Mempty
type instance Apply SFold (x ': xs) = x ## Apply SFold xs
type Fold xs = Apply SFold xs

data SConstR :: a ~> b ~> b
type instance Apply SConstR a = IdSym0

-- Parser type

type TChar = Symbol

data Parser (a :: *) where
  PAnyChar :: Parser TChar
  PChar :: TChar -> Parser TChar
  PMap :: (a ~> b) -> Parser a -> Parser b
  PAp :: Parser (a ~> b) -> Parser a -> Parser b
  PPure :: a -> Parser a
  PAlt :: Parser a -> Parser a -> Parser a
  PSome :: Parser a -> Parser [a]
  PMany :: Parser a -> Parser [a]
  PTry :: Parser a -> Parser a
  PLabel :: Parser a -> Symbol -> Parser a
  PText :: Symbol -> Parser Symbol
  PSatisfy :: (TChar ~> Bool) -> Parser TChar
  EOF :: Parser ()

data Err = Unknown | Expected Symbol Symbol
data Result (a :: *) = Success Bool a [TChar] | Fail Bool Err

type (<$>) = PMap
type (<*>) = PAp
type (<|>) = PAlt
type Pure = PPure
type instance (##) (p1 :: Parser a) (p2 :: Parser a) = MappendSym0 <$> p1 <*> p2
type (<*) p1 p2 = ConstSym0 <$> p1 <*> p2
type (*>) p1 p2 = SConstR <$> p1 <*> p2

type family RunParser (pa :: Parser a) (txt :: [Symbol]) :: Result a where
  RunParser PAnyChar xs = RunAnyChar xs
  RunParser (PChar a) xs = RunChar a xs
  RunParser (PMap f p) xs = RunMap f p xs
  RunParser (PAp pf pa) xs = RunAp pf pa xs
  RunParser (PPure a) xs = Success False a xs
  RunParser (PAlt p1 p2) xs = RunAlt p1 p2 xs
  RunParser (PSome p) xs = RunParser (TyCon2 '(:) <$> p <*> PMany p) xs
  RunParser (PMany p) xs = RunParser (PSome p <|> PPure '[]) xs
  RunParser (PTry f) xs = RunTry (RunParser f xs)
  RunParser (PLabel p label) xs = RunLabel (RunParser p xs) label xs
  RunParser (PText txt) xs = RunText txt xs
  RunParser (PSatisfy f) xs = RunSatisfy f xs
  RunParser EOF '[] = Success False '() '[]
  RunParser EOF (x ': xs) = Fail False (Expected "eof" (Fold (x ': xs)))

-- PChar
type family RunAnyChar xs where
  RunAnyChar (x ': xs) = Success True x xs
  RunAnyChar '[] = Fail False (Expected "any char" "")

-- PChar
type family RunChar a as where
  RunChar a (a ': as) = Success True a as
  RunChar a as = Fail False (Expected a (Fold as))

-- <$>
type family RunMap f p xs where
  RunMap f p xs = RunMap' f (RunParser p xs)

type family RunMap' f r where
  RunMap' f (Success e a xs) = Success e (f @@ a) xs
  RunMap' f (Fail e msg) = Fail e msg

-- <*>
type family RunAp pf pa xs where
  RunAp pf pa xs = RunAp1 (RunParser pf xs) pa xs

type family RunAp1 pf pa xs where
  RunAp1 (Success e f xs) pa _ = RunAp2 e f (RunParser pa xs)
  RunAp1 (Fail e msg) _ _ = Fail e msg

type family RunAp2 e f pb where
  RunAp2 e1 f (Success e2 a xs) = Success (e1 || e2) (f @@ a) xs
  RunAp2 e1 f (Fail e2 msg) = Fail (e1 || e2) msg

-- <|>
type family RunAlt p1 p2 xs where
  RunAlt p1 p2 xs = RunAlt1 (RunParser p1 xs) p2 xs

type family RunAlt1 r1 p2 xs where
  RunAlt1 (Success e a xs) _ _ = Success e a xs
  RunAlt1 (Fail True msg) _ _ = Fail True msg
  RunAlt1 (Fail False m1) p2 xs = RunAlt2 m1 (RunParser p2 xs)

type family RunAlt2 m1 r2 where
  RunAlt2 m1 (Success e a xs) = Success e a xs
  RunAlt2 m1 (Fail True m2) = Fail True m2
  RunAlt2 m1 (Fail False m2) = Fail False (CombineErr m1 m2)

type family CombineErr m1 m2 where
  CombineErr Unknown m2 = m2
  CombineErr m1 Unknown = m1
  CombineErr (Expected a1 xs) (Expected a2 xs) = Expected (a1 ## ", " ## a2) xs

-- Try
type family RunTry r where
  RunTry (Success e a xs) = Success e a xs
  RunTry (Fail False msg) = Fail False msg
  RunTry (Fail True msg) = Fail False Unknown

type family RunLabel r label xs where
  RunLabel (Success e a xs) _ _ = Success e a xs
  RunLabel (Fail e _) label xs = Fail e (Expected label (Fold xs))

type family RunText txt xs where
  RunText txt xs = RunText' txt xs (ToList txt) xs

type family RunText' pat start as xs where
  RunText' pat start '[] xs = Success True pat xs
  RunText' pat start (a ': as) (a ': xs) = RunText' pat start as xs
  RunText' pat start _ _ = Fail False (Expected pat (Fold start))

type family RunSatisfy f xs where
  RunSatisfy f '[] = Fail False Unknown
  RunSatisfy f (x ': xs) = RunSatisfy' (f @@ x) x xs

type family RunSatisfy' b x xs where
  RunSatisfy' True x xs = Success True x xs
  RunSatisfy' False x xs = Fail False Unknown

type SomeText p = SFold <$> PSome p
type ManyText p = SFold <$> PMany p


data SRange :: TChar -> TChar -> TChar ~> Bool
type instance Apply (SRange a b) c = a <= c && c <= b

type PRange a b = PSatisfy (SRange a b) `PLabel` (Fold [a, "-", b])

type Spaces = SFold <$> PMany (PChar " " <|> PChar "\n")

data SAnyOf :: Symbol -> TChar ~> Bool
type instance Apply (SAnyOf as) x = RunAnyOf (ToList as) x
type family RunAnyOf as x where
  RunAnyOf '[] x = False
  RunAnyOf (x ': as) x = True
  RunAnyOf (a ': as) x = RunAnyOf as x

type NoneOf xs = PSatisfy (NotSym0 .@#@$$$ SAnyOf xs)

type PNotChar c = PSatisfy (SNotEqual c)
type SNotEqual a = (/=@#@$$) a

data Specifier = Lit Symbol | Spec Symbol

type PSpecs = PSome ((TyCon1 Lit <$> SomeText (NoneOf "%")) <|> (TyCon1 Spec <$> (PChar "%" ## PRange "a" "z")))
