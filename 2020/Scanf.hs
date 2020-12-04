{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scanf where

import           Control.Applicative
import           Control.Monad
import           Data.Kind
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Typeable as Ty
import           Text.Parser.Char
import           Text.Parser.Combinators hiding (count)
import           Text.Parser.LookAhead

import           Util
import qualified Util.Text as T

import           RegexPCRE (regex)

class Scanf a where
  scanf' :: Parser (Parser a)

newtype WithRead a = WithRead a
  deriving (Show, Eq, Ord)

scanLit :: Parser (Parser a) -> Parser (Parser a)
scanLit sub = (do lits <- someText (noneOf "%")
                  pRest <- sub
                  return (text lits >> pRest))
              <|>
              (do try (text "%%")
                  pRest <- sub
                  return (text "%" >> pRest))

scanReadRegex :: Read a => Parser (Parser a)
scanReadRegex =
  (do pat <- try (text "%/" *> someText (noneOf "/") <* text "/r")
      return (read . T.unpack <$> regex pat)) <?> "%/regex/r"

scanRegex :: Parser (Parser Text)
scanRegex =
  (do pat <- try (text "%/" *> someText (noneOf "/") <* text "/s")
      return (regex pat)) <?> "%/regex/s"

-- This is pretty bad, it does infinite lookahead
parse_reads :: forall a. Read a => Parser a
parse_reads = do str <- lookAhead (some anyChar)
                 case reads str of
                   [(a :: a, leftover)] -> let n = length str - length leftover
                                      in read <$> string (take n str)
                   _ -> mzero

scanReadS :: forall a. (Ty.Typeable a, Read a) => Parser (Parser a)
scanReadS =
  (do pat <- try (text "%r")
      return (parse_reads <?> label))
  where label = show (Ty.typeOf (undefined :: a))

-- Eats literals, applies one of the supplied format specifiers, then
-- finishes off any following literals.
makeScanf1 :: Parser (Parser a) -> Parser (Parser a)
makeScanf1 spec = go
  where
    go = scanLit go <|> go1
    go1 = do here <- spec
             p_rest <- scanf' :: Parser (Parser ())
             return (here <* p_rest)

instance Scanf () where
  scanf' = scanLit scanf' <|> return (return ())

instance Scanf Int where
  scanf' = makeScanf1 (
    (do try (text "%i")
        let p_here = do
              may_minus <- optional (char '-')
              num <- read <$> some digit
              return $ case may_minus of
                Just _ -> (-num)
                Nothing -> num
        return p_here)
    <|> scanReadRegex
    <|> scanReadS)

instance Scanf Double where
  scanf' = makeScanf1 (
    scanReadRegex <|>
    scanReadS)

instance Scanf Char where
  scanf' = makeScanf1 $ do
    try (text "%c")
    return anyChar

instance Scanf Bool where
  scanf' = makeScanf1 $ do
    scanReadS

instance Scanf Text where
  scanf' = makeScanf1 scanRegex

instance (Ty.Typeable a, Read a) => Scanf (WithRead a) where
  scanf' = makeScanf1 (fmap WithRead <$> (scanReadRegex <|> scanReadS))

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
