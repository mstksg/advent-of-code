import Data.Foldable
import Data.Monoid
import Data.Bool
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as V

type Stuff = U.Vector Bool

pack :: String -> Stuff
pack cs = V.fromList [c == '1' | c <- cs]

unpack :: Stuff -> String
unpack bs = map (bool '0' '1') (V.toList bs)

expand :: Stuff -> Stuff
expand xs = fold [xs, V.fromList [False], V.map not (V.reverse xs)]

expandTo :: Int -> Stuff -> Stuff
expandTo n bs
  | V.length bs >= n = V.take n bs
  | otherwise        = expandTo n (expand bs)

checksum :: Stuff -> Stuff
checksum bs
  | odd (V.length bs) = bs
  | otherwise = checksum $ V.generate (V.length bs `div` 2) $ \n ->
      bs V.! (2*n) == bs V.! (2*n+1)
