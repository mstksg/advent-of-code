#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Functor
import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Hashable
import           Data.Heap (MinHeap)
import qualified Data.Heap as H
import           Data.List
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Ord
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Generics
import           System.IO.Unsafe
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util.Util

data DamageType = Cold | Fire | Radiation | Slashing | Bludgeoning
  deriving (Show, Eq, Ord)

data Weakness = Weak | Normal | Immune
  deriving (Show, Eq, Ord)

data Team = Imm | Inf
  deriving (Show, Eq, Ord)

newtype Id = Id Int
  deriving (Show, Eq, Ord)

data Group = Group {
  gId :: Id,
  gUnits :: Int,
  gHP :: Int,
  gWeakness :: Map DamageType Weakness,
  gAttack :: Int,
  gAttackType :: DamageType,
  gInitiative :: Int,
  gTeam :: Team
  }
  deriving (Show, Eq, Ord)

-- 3108 units each with 2902 hit points (weak to bludgeoning; immune to slashing, fire) with an attack that does 7 cold damage at initiative 13

pWeakness :: (CharParsing m) => m Weakness
pWeakness = (Weak <$ string "weak") <|>
            (Immune <$ string "immune")

pType :: (CharParsing m) => m DamageType
pType = (Cold <$ string "cold") <|>
        (Fire <$ string "fire") <|>
        (Radiation <$ string "radiation") <|>
        (Slashing <$ string "slashing") <|>
        (Bludgeoning <$ string "bludgeoning")

pGroup :: (CharParsing m, Monad m) => m Group
pGroup = do units <- number
            string " units each with "
            hp <- number
            string " hit points "
            weak <- option Map.empty $ between (char '(') (char ')') $ do
              let pEntry = do
                    w <- pWeakness
                    string " to "
                    ts <- sepBy pType (string ", ")
                    return (Map.fromList [(t, w) | t <- ts])
              fmap fold $ sepBy pEntry (string "; ")
            spaces
            string "with an attack that does "
            attack <- number
            spaces
            attacktype <- pType
            string " damage at initiative "
            initiative <- number
            return Group{
              gId = Id initiative,
              gUnits = units,
              gHP = hp,
              gWeakness = weak,
              gAttack = attack,
              gAttackType = attacktype,
              gInitiative = initiative,
              gTeam = undefined
              }

pInput :: (CharParsing m, Monad m) => m (Map Id Group)
pInput = do string "Immune System:"
            spaces
            groups1 <- some (try pGroup <* spaces)
            let groups1' = [g { gTeam = Imm } | g <- groups1]
            string "Infection:"
            spaces
            groups2 <- some (try pGroup <* spaces)
            let groups2' = [g { gTeam = Inf } | g <- groups2]
            return (Map.fromList [(gId g, g) | g <- groups1' ++ groups2'])

type Battle = Map Id Group

effectivePower :: Group -> Int
effectivePower g = gUnits g * gAttack g

-- effectiveDamage a b = damage which a would deal to b, accounting for attack type
effectiveDamage :: Group -> Group -> Int
effectiveDamage g tgt = effectivePower g * adjust (Map.findWithDefault Normal (gAttackType g) (gWeakness tgt))
  where
    adjust Immune = 0
    adjust Normal = 1
    adjust Weak = 2

chooseTargets :: Battle -> Map Id Id
chooseTargets groups = go choiceOrder (Map.keysSet groups)
  where
    choiceOrder = sortOn (Down . (effectivePower &&& gInitiative)) (Map.elems groups)
    go [] _ = Map.empty
    go (g:gs) available
      | null targets               = go gs available
      | effectiveDamage g tgt == 0 = go gs available
      | otherwise                  = --(trace $ "Group " ++ show (gId g) ++ " will attack " ++ show (gId tgt) ++ " for " ++ show (effectiveDamage g tgt) ++ " damage.") $
                                     Map.insert (gId g) (gId tgt) $ go gs (Set.delete (gId tgt) available)
      where
        targets = [t | tid <- toList available, let t = groups Map.! tid, gTeam t /= gTeam g]
        tgt = maximumOn (\t -> (effectiveDamage g t, effectivePower t, gInitiative t)) targets

attack :: Group -> Group -> Group
attack g tgt = --trace ("Group " ++ show (gId g) ++ " attacks " ++ show (gId tgt) ++ ", killing " ++ show unitsLost ++ " units.") $
  tgt { gUnits = gUnits tgt - unitsLost }
  where
    unitsLost = min (gUnits tgt) (effectiveDamage g tgt `div` gHP tgt)

fight :: Battle -> Battle
fight groups = go attackOrder groups
  where
    targets = chooseTargets groups
    attackOrder = sortOn (Down . gInitiative . (groups Map.!) . fst) (Map.toList targets)

    go [] groups = groups
    go ((attackerId, defenderId) : ts) groups = case Map.lookup attackerId groups of
      Just attacker -> go ts groups'
        where
          defender = groups Map.! defenderId
          defender' = attack attacker defender
          groups' = if gUnits defender' > 0 then
                      Map.insert defenderId defender' groups
                    else
                      Map.delete defenderId groups
      Nothing -> go ts groups -- died

fightUntilWon :: Battle -> Battle
fightUntilWon groups
  | groups' == groups = groups
  | otherwise = fightUntilWon groups'
  where
    groups' = fight groups

data Outcome = InfectionWin Int | Stalemate | ImmuneWin Int
  deriving (Show, Eq, Ord)

outcome :: Battle -> Outcome
outcome groups = case teams of
  [Imm] -> ImmuneWin units
  [Inf] -> InfectionWin units
  [_, _] -> Stalemate
  where
    teams = nub $ [gTeam g | g <- toList groups]
    units = sum [gUnits g | g <- toList groups]

solve1 :: Battle -> Outcome
solve1 groups = outcome (fightUntilWon groups)

solve2 :: Battle -> Outcome
solve2 groups = head [s | n <- [0..], let s = solve1 (boost n groups), s >= ImmuneWin 0]
  where
    boost n groups = (\g -> if gTeam g == Imm then g { gAttack = gAttack g + n } else g) <$> groups


main :: IO ()
main = do
  txt <- readFile "input/24.txt"
  let groups = parse pInput txt
  print (nub [gTeam g | g <- toList groups])
  print $ solve1 groups
  print $ solve2 groups
