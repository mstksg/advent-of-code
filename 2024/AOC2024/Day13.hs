{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC2024.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.
module AOC2024.Day13 (
  day13a,
  day13b,
  getPrize'',
  testPrize'',
  solveBezout,
  restrictRange,
  runBezout,
  iterateRange,
  applyRanges,
)
where

import AOC.Prelude
import qualified Data.Graph.Inductive as G
import Data.Proxy
import qualified Data.IntMap as IM
import qualified Data.IntMap.NonEmpty as IM
import qualified Data.IntSet as IS
import qualified Data.IntSet.NonEmpty as NEIS
import qualified Data.Interval as IV
import qualified Data.List.NonEmpty as NE
import qualified Data.List.PointedList as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map as M
import qualified Data.Map.NonEmpty as NEM
import qualified Data.OrdPSQ as PSQ
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Linear as L
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PP

-- Button A: X+53, Y+35
-- Button B: X+11, Y+23
-- Prize: X=13386, Y=18840

-- a x + b y = z
-- a x = z - b y

getPrize :: V2 (V2 Int) -> V2 Int -> Maybe Int
getPrize (V2 a b) g =
  minimumMay
    [ na * 3 + nb
    | na <- [0 .. 100]
    , nb <- [0 .. 100]
    , na *^ a + nb *^ b == g
    ]

-- Button A: X+69, Y+48
-- Button B: X+41, Y+88
-- Prize: X=5242, Y=3944

day13a :: _ :~> _
day13a =
  MkSol
    { sParse = parseMaybe' $ flip P.sepBy "\n\n" $ do
        "Button A: X+"
        x <- pDecimal
        ", Y+"
        y <- pDecimal
        P.newline
        "Button B: X+"
        x' <- pDecimal
        ", Y+"
        y' <- pDecimal
        P.newline
        "Prize: X="
        x'' <- pDecimal
        ", Y="
        y'' <- pDecimal
        pure (V3 (V2 x y) (V2 x' y') (V2 x'' y''))
    , sShow = show
    , sSolve =
        noFail $
          sum . mapMaybe getPrize''
    }

-- a x + b y = z
-- a x = z - b y
-- a = (z - by) / x

-- a = z/x - b*y/x

getPrize' :: V2 (V2 Int) -> V2 Int -> Maybe Int
getPrize' (V2 a@(V2 ax _) b) g =
  listToMaybe
    [ na * 3 + nb
    | nb <- takeWhile zby [0 ..]
    , let V2 zbx' _ = g - nb *^ b
    , zbx' `mod` ax == 0
    , let na = zbx' `div` ax
    , na *^ a + nb *^ b == g
    ]
  where
    zby nb = all (>= 0) $ g - nb *^ b

-- kx - xFactor * r' > 0
-- kx > xFactor * r'
-- r' < kx / xFactor, xFactor > 0
-- r' > kx / xFactor, xFactor < 0

-- ky + yFactor * r' > 0
-- yFactor * r' > -ky
-- r' > - kx / yFactor, yFactor > 0
-- r' < - kx / yFactor, yFactor < 0

--
data BezoutSol = BS
  { bsA :: V2 Int
  -- ^ m r + b
  , bsB :: V2 Int
  -- ^ m r + b
  }
  deriving stock Show

-- deriving stock instance Show (f Point) => Show (BezoutSol f)

-- solvePositiveBezout :: V3 Int -> Maybe (BezoutSol Identity)
-- solvePositiveBezout (V3 x y z) = do
--   guard $ dm == 0
--   (kx, ky) <- firstJust checkK [0 ..]
--   let xFactor = y `div` d
--       yFactor = x `div` d
--       rxRange
--         | xFactor > 0 = IV.NegInf IV.<=..<= IV.Finite (kx `div` xFactor)
--         | otherwise = IV.Finite (kx `div` xFactor) IV.<=..<= IV.PosInf
--       ryRange
--         | yFactor > 0 = IV.Finite (-(ky `div` yFactor)) IV.<=..<= IV.PosInf
--         | otherwise = IV.NegInf IV.<=..<= IV.Finite (-(ky `div` yFactor))
--       rRange = rxRange `IV.intersection` ryRange
--   (IV.Finite maxR, IV.Closed) <- pure $ IV.lowerBound' rRange
--   (IV.Finite minR, IV.Closed) <- pure $ IV.upperBound' rRange
--   pure
--     BS
--       { bsRange = Identity $ V2 minR maxR
--       , bsA = V2 (-xFactor) kx
--       , bsB = V2 yFactor ky
--       }
--   where
--     d = gcd x y
--     (dd, dm) = z `divMod` d
--     checkK i = [(dd * i, dd * zd) | zm == 0]
--       where
--         (zd, zm) = (d - i * x) `divMod` y

-- kx - xFactor * r' > min
-- xFactor * r' < kx - min
-- r' < (kx - min) / xFactor , xFactor > 0
-- r' < (kx - min) / xFactor , xFactor < 0

-- ky + yFactor * r' > min
-- yFactor * r' > min - ky
-- r' > (min - ky) / yFactor, yFactor > 0
-- r' < (min - kx) / yFactor, yFactor < 0

-- | Range of R such that a/b are within the given interval
restrictRange :: IV.Interval Double -> IV.Interval Double -> BezoutSol -> IV.Interval Double
restrictRange ivA ivB BS{..} = IV.intersections [minA, maxA, minB, maxB]
  where
    V2 ma ba = fromIntegral <$> bsA
    V2 mb bb = fromIntegral <$> bsB
    (minBoA, minClA) = IV.lowerBound' ivA
    (maxBoA, maxClA) = IV.upperBound' ivA
    (minBoB, minClB) = IV.lowerBound' ivB
    (maxBoB, maxClB) = IV.upperBound' ivB
    minALim = minBoA <&> \mbo -> (mbo - ba) / ma
    maxALim = maxBoA <&> \mbo -> (mbo - ba) / ma
    minBLim = minBoB <&> \mbo -> (mbo - bb) / mb
    maxBLim = maxBoB <&> \mbo -> (mbo - bb) / mb
    minA = mkIval (ma > 0) (minALim, minClA)
    maxA = mkIval (ma < 0) (maxALim, maxClA)
    minB = mkIval (mb > 0) (minBLim, minClB)
    maxB = mkIval (mb < 0) (maxBLim, maxClB)
    mkIval isGT l
      | isGT = IV.interval (mkNeg `first` l) (IV.PosInf, IV.Open)
      | otherwise = IV.interval (IV.NegInf, IV.Open) (mkPos `first` l)
    mkNeg = \case
      IV.PosInf -> IV.NegInf
      c -> c
    mkPos = \case
      IV.NegInf -> IV.PosInf
      c -> c

-- | Given range of R, give ranges of a/b
applyRanges :: IV.Interval Double -> BezoutSol -> V2 (IV.Interval Double)
applyRanges ivR BS{..} = V2 (IV.mapMonotonic (+ ba) $ scaleInterval ma ivR) (IV.mapMonotonic (+ bb) $ scaleInterval mb ivR)
  where
    V2 ma ba = fromIntegral <$> bsA
    V2 mb bb = fromIntegral <$> bsB

scaleInterval :: (Ord a, Num a) => a -> IV.Interval a -> IV.Interval a
scaleInterval x iv
    | x > 0 = IV.interval ((* x) <$> minBo, minCl) ((* x) <$> maxBo, maxCl)
    | otherwise = IV.interval ((* x) <$> flipInf maxBo, maxCl) ((* x) <$> flipInf minBo, minCl)
  where
    (minBo, minCl) = IV.lowerBound' iv
    (maxBo, maxCl) = IV.upperBound' iv
    flipInf = \case
      IV.NegInf -> IV.PosInf
      IV.PosInf -> IV.PosInf
      IV.Finite q -> IV.Finite q

solveBezout :: V3 Int -> Maybe BezoutSol
solveBezout (V3 x y z) = do
  guard $ dm == 0
  (kx, ky) <- firstJust checkK [0 ..]
  let xFactor = y `div` d
      yFactor = x `div` d
  pure
    BS
      { bsA = V2 (-xFactor) kx
      , bsB = V2 yFactor ky
      }
  where
    d = gcd x y
    (dd, dm) = z `divMod` d
    checkK i = [(dd * i, dd * zd) | zm == 0]
      where
        (zd, zm) = (d - i * x) `divMod` y

runBezout :: BezoutSol -> Int -> V2 Int
runBezout BS{..} r = (\(V2 m b) -> m * r + b) <$> V2 bsA bsB

-- okay now we want to find r, r' where
-- m r + b == m' r' + b'
-- m r - m' r' = b' - b
-- lol this becomes a second bezout
getPrize'' :: V3 (V2 Int) -> Maybe Int
getPrize'' (V3 a b g) = do
    -- bsx: rx -> valid ax/bx/gx solutions
    -- bsy: ry -> valid ay/by/gy solutions
    V2 bsx bsy <- traverse solveBezout . sequenceA $ V3 a b g
    -- rXs: rx giving valid solutions
    -- rYs: ry giving valid solutions
    let rXs = restrictRange positive positive bsx
        rYs = restrictRange positive positive bsy
    guard $ not (IV.null rXs)
    guard $ not (IV.null rYs)
    -- how to generate r's that match for both rx and ry for A
    bsa <- solveBezout $ V3 (view _1 $ bsA bsx) (negate $ view _1 $ bsA bsy) (view _2 (bsA bsy) - view _2 (bsA bsx))
    -- -- how to generate r's that match for both rx and ry for B
    -- bsb <- solveBezout $ V3 (view _1 $ bsB bsx) (negate $ view _1 $ bsB bsy) (view _2 (bsB bsy) - view _2 (bsB bsx))
    rAs <- iterateRange $ restrictRange rXs rYs bsa
    -- rBs <- iterateRange $ restrictRange rXs rYs bsb
    listToMaybe [ xa * 3 + xb
         | V2 raA raB <- runBezout bsa <$> rAs
         -- , V2 rbA rbB <- runBezout bsb <$> rBs
         , let x = runBezout bsx raA
               y = runBezout bsy raB
               V2 xa xb = x
         -- , x == y
         ]
  where
    positive = 0 IV.<=..< IV.PosInf

-- okay now we want to find r, r' where
-- m r + b == m' r' + b'
-- m r - m' r' = b' - b
-- lol this becomes a second bezout
testPrize'' :: V3 (V2 Int) -> Maybe _
testPrize'' (V3 a b g) = do
    -- bsx: rx -> valid ax/bx/gx solutions
    -- bsy: ry -> valid ay/by/gy solutions
    V2 bsx bsy <- traverse solveBezout . sequenceA $ V3 a b g
    -- at this point we have ways to solve A ax + B bx = gx, A by + B by = gy
    -- but we want to know there are any (A,B) pair that would work for both.
    --
    -- A,B are of the form (ma r + ba, mb r + bb) for both. so if we want them
    -- to be equal, we need r where:
    --
    -- max rA + bax == may rA' + bay
    -- => max rA - may rA' == bay - bax
    -- mbx rB + bbx == mby rB' + bby
    -- => mbx rB - mbx rB' == bby - bbx
    --
    -- that gives us valid rA/rB's, but that space is still too huge. How do
    -- we link it back to the original xyz...
    --
    -- Ah okay yeah we must remember that each A and B are themselves related.
    -- how do we pick (A,B) now considering they work for both?
    --
    -- what can rA be? well:
    --
    -- (max rAx + bax) ax + (mbx rBx + bbx) bx = gx
    -- (may rAy + bay) ay + (mby rBy + bby) by = gy
    --
    -- every choice of rAx and rBx within rXs produces a valid 1st eq
    -- every choice of rAy and rBy within rXy produces a valid 2st eq
    --
    -- but rAx rAy cannot vary independently. we can parameterize possible
    -- (rAx, rAy) pairs by a variable qA. And same for (rBx rBy) by a variable
    -- qB. and then we want to know what ranges qA and qB can take.
    --
    -- But, even those ranges are way too big, we need some way to intercept
    -- them again. I guess if we plub them back in, we can get q to emerge
    -- from the big equation.
    --
    -- Actually wait that's wrong, they have to have the same R across (A,B),
    -- so that means our equations are actually:
    --
    -- (max rX + bax) ax + (mbx rX + bbx) bx = gx
    -- (may rY + bay) ay + (mby rY + bby) by = gy
    --
    -- and of course our A's have to be the same, which means we have:
    --
    -- max rX + bax == may rY + bay
    -- => max rX - may rY = bay - bax
    -- mbx rX + bbx == may rY + bby
    -- => mab rX - mab rY = bby - bbx
    --
    -- yeah this is starting to get more constrained. Okay now let's break
    -- this down into qA and qB... and see if that helps at all
    --
    -- max (max' qA + bax') - may (may' qA + bay') = bay - bax
    -- mbx (mbx' qB + bbx') - mby (mby' qB + bby') = bby - bbx
    --
    -- hey it looks like every time we go deeper, the range of values gets
    -- smaller. maybe there's something going on here.
    --
    -- rXs: rx giving valid solutions
    -- rYs: ry giving valid solutions
    let rXs = restrictRange positive positive bsx
        rYs = restrictRange positive positive bsy
    guard $ not (IV.null rXs)
    guard $ not (IV.null rYs)
    -- how to generate r's that match for both rx and ry for A
    bsa <- solveBezout $ V3 (view _1 $ bsA bsx) (negate $ view _1 $ bsA bsy) (view _2 (bsA bsy) - view _2 (bsA bsx))
    -- how to generate r's that match for both rx and ry for B
    bsb <- solveBezout $ V3 (view _1 $ bsB bsx) (negate $ view _1 $ bsB bsy) (view _2 (bsB bsy) - view _2 (bsB bsx))
    let qAs = restrictRange rXs rYs bsa
        qBs = restrictRange rXs rYs bsb
    bsa' <- solveBezout $ V3 (view _1 $ bsA bsa) (negate $ view _1 $ bsA bsb) (view _2 (bsA bsb) - view _2 (bsA bsa))
    bsb' <- solveBezout $ V3 (view _1 $ bsB bsa) (negate $ view _1 $ bsB bsb) (view _2 (bsB bsb) - view _2 (bsB bsa))
    let qAs' = restrictRange qAs qBs bsa'
        qBs' = restrictRange qAs qBs bsb'
    -- pure $ [ applyRanges rXs bsx, applyRanges rYs bsy ]
    -- pure $ [rXs, rYs, rAs, rBs]
    -- pure $ IV.mapMonotonic (round @Double @Int) <$> [rXs, rYs, rAs, rBs]
    pure $ IV.width <$> [rXs, rYs, qAs, qBs, qAs', qBs']
    -- minimumMay [ xa * 3 + xb
    --      | V2 raA raB <- runBezout bsa <$> rAs
    --      -- , V2 rbA rbB <- runBezout bsb <$> rBs
    --      , let x = runBezout bsx raA
    --            y = runBezout bsy raB
    --            V2 xa xb = x
    --      , x == y
    --      ]
    -- listToMaybe [ V2 xa xb
    --      | V2 raA raB <- map (runBezout bsa) $ fold $ iterateRange rAs
    --      -- , V2 rbA rbB <- runBezout bsb <$> rBs
    --      , let x = runBezout bsx raA
    --            y = runBezout bsy raB
    --            V2 xa xb = x
    --      -- , x == y
    --      ]
  where
    positive = 0 IV.<=..< IV.PosInf


-- | Nothing if range is infinite
iterateRange :: IV.Interval Double -> Maybe [Int]
iterateRange iv = case (IV.lowerBound' iv, IV.upperBound' iv) of
  ((IV.Finite mn, _), (IV.Finite mx, _)) -> Just [ceiling mn .. floor mx]
  _ -> Nothing


  -- [ (candA *^ a + candB *^ b)
  -- | let dx = gcd ax bx
  --       (dxd, dxm) = gx `divMod` dx
  -- , dxm == 0
  -- , (kax, kbx) <- maybeToList $ firstJust (checkK dx) [0 ..]
  -- , rx <- [0 .. 10]
  -- , let candA = dxd * kax - ((bx `div` dx) * rx)
  --       candB = dxd * kbx + ((ax `div` dx) * rx)
  --       -- nb <- takeWhile zby [0 .. ]
  --       -- , let V2 zbx' _ = g - nb *^ b
  --       -- , zbx' `mod` ax == 0
  --       -- , let na = zbx' `div` ax
  --       -- , na *^ a + nb *^ b == g
  -- ]
  -- where
  --   checkK dx i = [(i, d) | m == 0]
  --     where
  --       (d, m) = (dx - i * ax) `divMod` bx

--   -- where
--   --   zby nb = all (>= 0) $ g - nb *^ b

day13b :: _ :~> _
day13b =
  MkSol
    { sParse = sParse day13a
    , sShow = show
    , sSolve =
        noFail $
          sum . mapMaybe getPrize'' . map (over _z (10000000000000 *^))
    }
