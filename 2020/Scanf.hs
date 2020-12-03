{-# LANGUAGE OverloadedStrings #-}

module Scanf where

import           Control.Applicative
import           Data.Kind
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)

import           Util
import qualified Util.Text as T

class Scanf a where
  scanf' :: Parser (Parser a)

instance Scanf () where
  scanf' = (do lits <- someText (noneOf "%")
               pRest <- scanf'
               return (text lits >> pRest))
           <|>
           (do try (text "%%")
               pRest <- scanf'
               return (text "%" >> pRest))
           <|> return (return ())

instance Scanf Int where
  scanf' = (do lits <- someText (noneOf "%")
               pRest <- scanf'
               return (text lits >> pRest))
           <|>
           (do try (text "%%")
               pRest <- scanf'
               return (text "%" >> pRest))
           <|>
           (do try (text "%i")
               pRest <- scanf' :: Parser (Parser ())
               let p_here = do
                     may_minus <- optional (char '-')
                     num <- read <$> some digit
                     return $ case may_minus of
                       Just _ -> (-num)
                       Nothing -> num
               return (p_here <* pRest))

instance Scanf Char where
  scanf' = (do lits <- someText (noneOf "%")
               pRest <- scanf'
               return (text lits >> pRest))
           <|>
           (do try (text "%%")
               pRest <- scanf'
               return (text "%" >> pRest))
           <|>
           (do try (text "%c")
               pRest <- scanf' :: Parser (Parser ())
               let p_here = anyChar
               return (p_here <* pRest))

instance Scanf Text where
  scanf' = (do lits <- someText (noneOf "%")
               pRest <- scanf'
               return (text lits >> pRest))
           <|>
           (do try (text "%%")
               pRest <- scanf'
               return (text "%" >> pRest))
           <|>
           (do try (text "%/[a-z]+/")
               pRest <- scanf' :: Parser (Parser ())
               let p_here = someText letter
               return (p_here <* pRest))

instance (Scanf a, Scanf b) => Scanf (a, b) where
  scanf' = do p1 <- scanf'
              p2 <- scanf'
              return ((,) <$> p1 <*> p2)

instance (Scanf a, Scanf b, Scanf c) => Scanf (a, b, c) where
  scanf' = do p1 <- scanf'
              p2 <- scanf'
              p3 <- scanf'
              return ((,,) <$> p1 <*> p2 <*> p3)

instance (Scanf a, Scanf b, Scanf c, Scanf d) => Scanf (a, b, c, d) where
  scanf' = do p1 <- scanf'
              p2 <- scanf'
              p3 <- scanf'
              p4 <- scanf'
              return ((,,,) <$> p1 <*> p2 <*> p3 <*> p4)

scanf :: Scanf a => Text -> Parser a
scanf fmt = parse (scanf' <* eof) fmt

-- From day 2:
-- λ> parse (scanf "%i-%i %c: %/[a-z]+/") "4-5 t: ftttttrvts" :: (Int, Int, Char, Text)
-- (4,5,'t',"ftttttrvts")
