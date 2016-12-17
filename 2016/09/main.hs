import Data.Char
import Data.Foldable
import Text.Parsec
import Data.Monoid

compressed :: Parsec String () String
compressed = fold <$> many (letter <|> expand)
  where
    number = read <$> many1 digit
    letter = pure <$> noneOf "("
    expand = do char '('
                size <- number
                char 'x'
                num <- number
                char ')'
                part <- count size anyToken
                return (fold $ replicate num part)

compressed' :: Parsec String () (Sum Int)
compressed' = fold <$> many (letter <|> expand)
  where
    number = read <$> many1 digit
    letter = Sum 1 <$ noneOf "("
    expand = do char '('
                size <- number
                char 'x'
                num <- number
                char ')'
                part <- count size anyToken
                let Right dsize = parse compressed' "" part
                return (Sum num * dsize)

part1 :: String -> String
part1 text = case parse compressed "" clean of
               Right s -> show (length s)
  where
    clean = filter (not . isSpace) text


part2 :: String -> Sum Int
part2 text = case parse compressed' "" clean of
               Right s -> s
  where
    clean = filter (not . isSpace) text
