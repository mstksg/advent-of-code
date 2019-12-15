{-# Language ConstraintKinds #-}
module Util where


import           Data.Char
import           Data.Constraint
import           Data.Foldable
import           Data.List
import           Data.Ord
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Trifecta as Trifecta

maximumOn :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maximumOn f = maximumBy (comparing f)

count :: (Foldable f, Eq a) => a -> f a -> Int
count x xs = getSum $ foldMap (\y -> if y == x then 1 else 0) xs

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

type Parser m = (CharParsing m, Monad m)

parse :: Trifecta.Parser a -> Text -> a
parse p txt = case Trifecta.parseString p mempty (T.unpack txt) of
  Trifecta.Success a -> a
  Trifecta.Failure e -> error (show (Trifecta._errDoc e))
