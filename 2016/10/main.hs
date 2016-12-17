{-# LANGUAGE BangPatterns #-}
import Debug.Trace
import Text.Parsec hiding (Empty)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

type Chip = Int
type BotID = Int

data BotState = Empty | One Chip
  deriving (Show)

data Target = Bot BotID
            | Output Int
            deriving (Show)

data Rule = Rule Target Target deriving (Show)

data Factory = Factory {
  bots :: Map BotID BotState,
  outputs :: Map Int [Chip]
  } deriving (Show)

giveChip :: Map BotID Rule -> Target -> Chip -> Factory -> Factory
giveChip rules (Bot botid) chip (Factory bots outputs) =
  case Map.lookup botid bots of
    Nothing -> Factory (Map.insert botid (One chip) bots) outputs
    Just Empty -> Factory (Map.insert botid (One chip) bots) outputs
    Just (One other) -> let bots' = Map.insert botid Empty bots
                            factory' = Factory bots' outputs
                            (lo, hi) = if chip < other then
                                         (chip, other) else (other, chip)
                            !_ = if (lo, hi) == (17, 61) then
                                   trace ("here! " ++ show botid ++ "\n") ()
                                 else ()
                        in case Map.lookup botid rules of
                             Just (Rule tlo thi) -> giveChip rules thi hi
                                                    $ giveChip rules tlo lo
                                                    $ factory'
                             Nothing -> error ("missing rule for " ++ show botid)

giveChip rules (Output n) chip (Factory bots outputs) = Factory bots outputs'
  where
    outputs' = Map.unionWith (++) (Map.singleton n [chip]) outputs

getRules :: String -> Map BotID Rule
getRules text = foldMap go (lines text)
  where
    number = read <$> many1 digit
    target :: Parsec String () Target
    target = Bot <$ string "bot " <*> number
             <|> Output <$ string "output " <*> number
    rule_line :: Parsec String () (BotID, Rule)
    rule_line = do string "bot "
                   id <- number
                   string " gives low to "
                   tlo <- target
                   string " and high to "
                   thi <- target
                   return (id, Rule tlo thi)

    go line = case parse rule_line "" line of
                Right (id, rule) -> Map.singleton id rule
                Left _ -> Map.empty

getChips :: String -> [(BotID, Chip)]
getChips text = foldMap go (lines text)
  where
    number = read <$> many1 digit
    rule_line :: Parsec String () (BotID, Chip)
    rule_line = do string "value "
                   chip <- number
                   string " goes to bot "
                   id <- number
                   return (id, chip)

    go line = case parse rule_line "" line of
                Right c -> [c]
                Left _ -> []

initial :: Factory
initial = Factory Map.empty Map.empty

part1 :: String -> String
part1 text = show $ foldl (\f (bot, chip) -> giveChip rules (Bot bot) chip f) initial chips
  where
    rules = getRules text
    chips = getChips text
