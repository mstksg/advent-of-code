import Control.Arrow (first)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

oneround :: [Int] -> [Int]
oneround xs
  | odd (length xs) = tail (half xs)
  | otherwise = half xs
  where
    half [] = []
    half (x:xs) = x : half' xs
    half' [] = []
    half' (x:xs) = half xs

initial :: Int -> Seq Int
initial n = Seq.fromList [1..n]

step :: Seq a -> Either a (Seq a)
step elves
  | Seq.null xs = Left a
  | otherwise = Right ((l Seq.>< r) Seq.|> a)
  where
    (a, xs) = case Seq.viewl elves of
                a Seq.:< xs -> (a, xs)
                Seq.EmptyL -> error "out of elves"
    (lb, r) = Seq.splitAt (Seq.length elves `div` 2) xs
    l = case Seq.viewr lb of
          l Seq.:> _ -> l

iterateM :: Monad m => (a -> m a) -> a -> m a
iterateM f a = do a' <- f a
                  iterateM f a'

iterateE :: (a -> Either b a) -> a -> ([a], b)
iterateE f a = case f a of
                 Right a' -> let ~(as, b) = iterateE f a'
                             in (a:as, b)
                 Left b -> ([], b)

half :: [a] -> [a]
half [] = []
half [x] = []
half (x:y:xs) = x : half xs

main :: IO ()
main = do
  print $ first (half . half . half . map (Seq.take 1)) $ iterateE step $ initial 3001330
