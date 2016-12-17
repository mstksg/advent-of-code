import Data.Monoid
import Data.List

times :: Int -> Int -> [Int]
times size i = dropWhile (<0) [size * n - i | n <- [0..]]

data The a = None | The a | Top
  deriving (Show, Eq, Ord)

instance Eq a => Monoid (The a) where
  mempty = None
  mappend None x = x
  mappend x None = x
  mappend Top _ = Top
  mappend _ Top = Top
  mappend (The a) (The b) = if a == b then
                              The a
                            else Top

found :: [[Int]] -> Int
found times = case foldMap The heads of
                The t -> t
                _     -> found times'
  where
    mx = maximum $ head (transpose times)
    times' = map (dropWhile (<mx)) times
    heads = head (transpose times')

inputTimes :: [(Int, Int)] -> [[Int]]
inputTimes sx = [times s (x + i) | (i, (s, x)) <- zip [1..] sx]

part1 = inputTimes [(13, 1), (19, 10), (3, 2), (7, 1), (5, 3), (17, 5)]
part2 = inputTimes [(13, 1), (19, 10), (3, 2), (7, 1), (5, 3), (17, 5), (11, 0)]
