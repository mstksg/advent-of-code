{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : AOC.Solver
-- Copyright   : (c) Justin Le 2021
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Types to drive the challenge runner and help speed up/clean up
-- solutions.
module AOC.Solver (
  (:~>) (..),
  noFail,
  mapSum,
  traverseSum,
  withSolver,
  withSolver',
  SomeSolution (.., MkSomeSol),
  SolutionError (..),
  runSolution,
  runSomeSolution,
  ssIsNF,

  -- * 'DynoMap'
  runSolutionWith,
  runSomeSolutionWith,
  dyno,
  dyno_,
  TestType (..),
)
where

import AOC.Util
import AOC.Util.DynoMap
import Control.DeepSeq
import Data.Dependent.Sum
import Data.Foldable
import Data.Functor.Identity
import Data.Map (Map)
import Data.Semigroup (Sum (..))
import GHC.Generics (Generic)

-- | Abstracting over the type of a challenge solver to help with cleaner
-- solutions.
--
-- A @a ':~>' b@ encapsulates something that solves a challenge with input
-- type @a@ into a response of type @b@.
--
-- Consists of a parser, a shower, and a solver.  The solver solves
-- a general @a -> 'Maybe' b@ function, and the parser and shower are used
-- to handle the boilerplate of parsing and printing the solution.
data a :~> b = MkSol
  { sParse :: String -> Maybe a
  -- ^ parse input into an @a@
  , sSolve ::
      (?dyno :: DynoMap) =>
      a ->
      Maybe b
  -- ^ solve an @a@ input to a @b@ solution
  , sShow :: b -> String
  -- ^ print out the @b@ solution in a pretty way
  }

noFail :: (a -> b) -> a -> Maybe b
noFail f = Just . f

mapSum :: (Foldable t, Num b) => (a -> b) -> t a -> b
mapSum f = getSum . foldMap (Sum . f)

traverseSum :: (Foldable t, Applicative f, Num b) => (a -> f b) -> t a -> f b
traverseSum f = fmap sum . traverse f . toList

-- | Wrap an @a ':~>' b@ and hide the type variables so we can put
-- different solutions in a container.
data SomeSolution where
  MkSomeSolWH :: a :~> b -> SomeSolution
  MkSomeSolNF :: (NFData a, NFData b) => a :~> b -> SomeSolution

-- | Check if a 'SomeSolution' is equipped with an 'NFData' instance on the
-- types
ssIsNF :: SomeSolution -> Bool
ssIsNF = \case
  MkSomeSolWH _ -> False
  MkSomeSolNF _ -> True

data SomeSolHelp where
  SSH :: a :~> b -> SomeSolHelp

toHelp :: SomeSolution -> SomeSolHelp
toHelp (MkSomeSolWH x) = SSH x
toHelp (MkSomeSolNF x) = SSH x

-- | Handy pattern to work with both 'MkSomeSolWH' and 'MkSomeSolNF'.  As
-- a constructor, just uses 'MkSomeSolWH', so might not be desirable.
pattern MkSomeSol :: () => forall a b. () => a :~> b -> SomeSolution
pattern MkSomeSol s <- (toHelp -> SSH s)
  where
    MkSomeSol x = MkSomeSolWH x

{-# COMPLETE MkSomeSol #-}

-- | Errors that might happen when running a ':~>' on some input.
data SolutionError
  = SEParse
  | SESolve
  deriving stock (Show, Eq, Ord, Generic)

instance NFData SolutionError

-- | Construct a ':~>' from just a normal @String -> String@ solver.
-- Does no parsing or special printing treatment.
withSolver' :: (String -> String) -> String :~> String
withSolver' f = withSolver (Just . f)

-- | Construct a ':~>' from a @String -> 'Maybe' String@ solver, which
-- might fail.  Does no parsing or special printing treatment.
withSolver :: (String -> Maybe String) -> String :~> String
withSolver f =
  MkSol
    { sParse = Just
    , sShow = id
    , sSolve = f
    }

-- | Run a ':~>' on some input.
runSolution :: a :~> b -> String -> Either SolutionError String
runSolution = runSolutionWith mempty

-- | Run a ':~>' on some input, with a map of dynamic values for testing
runSolutionWith ::
  -- | map of dynamic values for testing with 'lookupDyno'.
  Map String (DSum TestType Identity) ->
  a :~> b ->
  String ->
  Either SolutionError String
runSolutionWith dm MkSol{..} (stripNewline -> s) = do
  x <- maybeToEither SEParse . sParse $ s
  y <- maybeToEither SESolve . sSolve $ x
  pure $ sShow y
  where
    ?dyno = Dyno dm

-- | Run a 'SomeSolution' on some input.
runSomeSolution ::
  SomeSolution ->
  String ->
  Either SolutionError String
runSomeSolution = runSomeSolutionWith mempty

-- | Run a 'SomeSolution' on some input, with a map of dynamic values for
-- testing
runSomeSolutionWith ::
  -- | map of dynamic values for testing with 'lookupDyno'.
  Map String (DSum TestType Identity) ->
  SomeSolution ->
  String ->
  Either SolutionError String
runSomeSolutionWith dm (MkSomeSol c) = runSolutionWith dm c

-- | From a @?dyno@ Implicit Params, look up a value at a given key.  Meant
-- to be used with TypeApplications:
--
-- > 'dyno' @"hello"
--
-- This can be used within the body of 'sSolve', since it will always be
-- called with the implicit parameter.
--
-- When called on actual puzzle input, result will always be 'Nothing'.
-- But, for some test inputs, there might be supplied values.
--
-- This is useful for when some problems have parameters that are
-- different with test inputs than for actual inputs.
dyno ::
  forall a.
  (HasTestType a, ?dyno :: DynoMap) =>
  String ->
  Maybe a
dyno = (`lookupDyno` ?dyno)

-- | A version of 'dyno' taking a default value in case the key is not
-- in the map.  When called on actual puzzle input, this is always 'id'.
-- However, for some test inputs, there might be supplied values.
--
-- Meant to be used with TypeApplications:
--
-- > 'dyno_' @"hello" 7
--
-- This is useful for when some problems have parameters that are
-- different with test inputs than for actual inputs.
dyno_ ::
  forall a.
  (HasTestType a, ?dyno :: DynoMap) =>
  String ->
  -- | default
  a ->
  a
dyno_ str def = lookupDynoWith str def ?dyno
