{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module AOC.Util.DynoMap (
  DynoMap (..),
  lookupDyno,
  lookupDynoWith,
  TestType (..),
  HasTestType (..),
)
where

import Control.Monad
import Data.Constraint.Extras.TH
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.GADT.Compare
import Data.GADT.Show
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Type.Equality

data TestType :: Type -> Type where
  TTInt :: TestType Int
  TTString :: TestType String

deriving stock instance Show (TestType a)

instance GShow TestType where
  gshowsPrec = showsPrec

deriveArgDict ''TestType

instance GEq TestType where
  geq = \case
    TTInt -> \case
      TTInt -> Just Refl
      _ -> Nothing
    TTString -> \case
      TTString -> Just Refl
      _ -> Nothing

class HasTestType a where
  hasTestType :: TestType a

instance HasTestType Int where hasTestType = TTInt

instance HasTestType String where hasTestType = TTString

newtype DynoMap = Dyno {runDyno :: Map String (DSum TestType Identity)}
  deriving newtype (Semigroup, Monoid)

fromTestType ::
  forall a f.
  HasTestType a =>
  DSum TestType f ->
  Maybe (f a)
fromTestType (tt :=> x) = (`gcastWith` x) <$> (tt `geq` hasTestType @a)

-- | Lookup the value at a given key in a 'Dyno'.
--
-- > lookupDyno "hello"
lookupDyno ::
  forall a.
  HasTestType a =>
  String ->
  DynoMap ->
  Maybe a
lookupDyno sm =
  fmap runIdentity . fromTestType
    <=< M.lookup sm
      . runDyno

-- | Like 'lookupDyno', but with a default value to be returned if the key
-- is not found or has the wrong type.
lookupDynoWith ::
  forall a.
  HasTestType a =>
  String ->
  a ->
  DynoMap ->
  a
lookupDynoWith sm def = fromMaybe def . lookupDyno sm
