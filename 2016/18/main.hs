import Data.Monoid
import Data.Foldable

data Tile = S | T deriving (Show, Eq, Ord)

neighbors :: a -> [a] -> [[a]]
neighbors a ts = go a ts
  where
    go t [u] = [[t, u, a]]
    go t (u:v:xs) = [t, u, v] : go u (v:xs)

rule :: [Tile] -> Tile
rule [T, T, S] = T
rule [S, T, T] = T
rule [T, S, S] = T
rule [S, S, T] = T
rule [_, _, _] = S

pack :: String -> [Tile]
pack = map go
  where
    go '.' = S
    go '^' = T

unpack :: [Tile] -> String
unpack = map go
  where
    go S = '.'
    go T = '^'

extend :: [Tile] -> [Tile]
extend xs = map rule (neighbors S xs)

part1 :: String -> String
part1 input = show $ foldl' mappend mempty $ map (foldMap go) $ take 400000 (iterate extend firstrow)
  where
    [firstrow] = map pack (lines input)
    go S = Sum 1
    go T = Sum 0
