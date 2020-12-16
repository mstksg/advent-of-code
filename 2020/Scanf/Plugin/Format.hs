module Scanf.Plugin.Format (parseScanf) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Constraint
import           Data.Foldable
import           Data.Foldable
import           Data.List
import           Data.Ord
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta (Parser)
import qualified Text.Trifecta as Trifecta

import           Scanf.Specifier

pLits :: Parser (Spec String)
pLits = Lit <$> (some (noneOf "% " <|> (string "%%" >> pure '%')))

pRead :: Parser (Spec String)
pRead = const SpecRead <$> string "%r"

pSpace :: Parser (Spec String)
pSpace = const SpecSpace <$> (some (char ' '))

pSpec :: Parser (Spec String)
pSpec = pLits <|> pSpace <|> pRead

parseString :: Parser a -> String -> Either String a
parseString p txt = case Trifecta.parseString p mempty txt of
  Trifecta.Success a -> Right a
  Trifecta.Failure e -> Left (show (Trifecta._errDoc e))

parseScanf :: String -> Either String [Spec String]
parseScanf txt = parseString (many pSpec <* eof) txt
