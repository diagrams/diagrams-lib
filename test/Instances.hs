{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | instances for QuickCheck Arbitrary and approximate equality

module Instances where

import           Diagrams.Prelude
import           Numeric.Extras
import           Test.Tasty.QuickCheck (Arbitrary (..), Gen)
import qualified Test.Tasty.QuickCheck as QC

------------------------------------------------------------
    -- Approximate Comparison for Doubles, Points

epsilon :: Double
epsilon = 0.001

class Approx a where
  (=~) :: a -> a -> Bool

infix 4 =~

--instance (Fractional a, Ord a) => Approx a where
instance Approx Double where
  (=~) a b = abs (a - b) < epsilon

instance Approx Float where
    (=~) a b = abs (a - b) < 0.001

instance Approx n => Approx (V2 n) where
    z1 =~ z2 = (z1^._x) =~ (z2^._x) && (z1^._y) =~ (z2^._y)

instance Approx n => Approx (V3 n) where
    z1 =~ z2 = (z1^._x) =~ (z2^._x) && (z1^._y) =~ (z2^._y) && (z1^._z) =~ (z2^._z)

instance Approx (v n) => Approx (Point v n) where
    p =~ q = view _Point p =~ view _Point q

instance (Approx n, RealExtras n) => Approx (Angle n) where
    a =~ b = normA (a^.rad) =~ normA (b^.rad) where
      normA ang = let ang' = ang `fmod` pi in if ang' >= 0 then ang' else ang'+pi

instance Approx n => Approx (Offset Closed V2 n) where
    OffsetClosed v0 =~ OffsetClosed v1 = v0 =~ v1

instance Approx n => Approx (Segment Closed V2 n) where
    Linear o0 =~ Linear o1 = o0 =~ o1
    Cubic c0 d0 o0 =~ Cubic c1 d1 o1 = c0 =~ c1 && d0 =~ d1 && o0 =~ o1
    _ =~ _ = False
    -- The above is conservative:
    -- Cubic never equals Linear even if they describe the same points

instance Approx n => Approx (FixedSegment V2 n) where
    FLinear a0 b0 =~ FLinear a1 b1 = a0 =~ a1 && b0 =~ b1
    FCubic a0 b0 c0 d0 =~ FCubic a1 b1 c1 d1 = a0 =~ a1 && b0 =~ b1 && c0 =~ c1 && d0 =~ d1
    _ =~ _ = False

instance Approx n => Approx (Trail' Line V2 n) where
    l0 =~ l1 = and $ zipWith (=~) (lineSegments l0) (lineSegments l1)

instance Approx n => Approx (Trail' Loop V2 n) where
    l0 =~ l1 = fst (loopSegments l0) =~ fst (loopSegments l1)

instance (Approx n, Floating n, Ord n) => Approx (Trail V2 n) where
    t0 =~ t1 = and $ zipWith (=~) (trailSegments t0) (trailSegments t1)

instance (Approx a, Approx (Vn a), Num (N a), Additive (V a)) =>
         Approx (Located a) where
           a0 =~ a1 = (loc a0 .-. origin) =~ (loc a1 .-. origin) && unLoc a0 =~ unLoc a1

instance Approx a => Approx (Maybe a) where
    Nothing =~ Nothing = True
    Nothing =~ Just _ = False
    Just _ =~ Nothing = False
    Just l =~ Just r = l =~ r

-- These may be too general
instance Approx a => Approx [a] where
    a =~ b = and $ zipWith (=~) a b

instance (Approx a, Approx b) => Approx (a, b) where
    (a0, b0) =~ (a1,b1) = (a0 =~ a1) && (b0 =~ b1)

------------------------------------------------------------
-- Arbitrary instances for Points, Paths

instance Arbitrary n => Arbitrary (V2 n) where
    arbitrary = (^&) <$> arbitrary <*> arbitrary
    shrink (coords -> x :& y) = (^&) <$> shrink x <*> shrink y

instance Arbitrary n => Arbitrary (V3 n) where
    arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary
    shrink (coords -> x :& y :& z) = V3 <$> shrink x <*> shrink y <*> shrink z

instance Arbitrary (v n) => Arbitrary (Point v n) where
    arbitrary = P <$> arbitrary
    shrink (P v) = P <$> shrink v

instance (Arbitrary n, Floating n, Ord n) => Arbitrary (Transformation V2 n) where
    arbitrary = QC.sized arbT
      where
        arbT 0 = return mempty
        arbT n = QC.oneof
          [ rotation    <$> arbitrary
          , scaling     <$> arbitrary
          , translation <$> arbitrary
          , reflectionAbout <$> arbitrary <*> arbitrary
          , (<>) <$> arbT (n `div` 2) <*> arbT (n `div` 2)
          ]

instance Arbitrary n => Arbitrary (Angle n) where
    arbitrary = review rad <$> arbitrary

instance (Arbitrary n, Floating n) => Arbitrary (Direction V2 n) where
    arbitrary = rotate <$> arbitrary <*> pure xDir

-- -- | Not a valid Show instance because not valid Haskell input
-- instance (Show n, RealFloat n) => Show (Direction V2 n) where
--     show d = "Dir" <> ( show $ d ^. _theta . turn )

-- NOTE on shrinks: Adding definitions of 'shrink' below seems to work
--   in simple tests, but test case failures hang for a very long time
--   (presumably trying lots and lots of expensive shrinks).  Not sure
--   how to make shrinking more tractable.

instance (Arbitrary a, Arbitrary (Vn a)) => Arbitrary (Located a) where
    arbitrary = at <$> arbitrary <*> arbitrary
--    shrink (viewLoc -> (p,a)) = uncurry at <$> shrink (a,p)

instance Arbitrary n => Arbitrary (Offset Closed V2 n) where
    arbitrary = OffsetClosed <$> arbitrary
--    shrink (OffsetClosed x) = OffsetClosed <$> shrink x

instance Arbitrary n =>  Arbitrary (Segment Closed V2 n) where
    arbitrary = QC.oneof [Linear <$> arbitrary, Cubic <$> arbitrary <*> arbitrary <*> arbitrary]
    -- shrink (Linear x) = Linear <$> shrink x
    -- shrink (Cubic x y z) = Linear z
    --                      : [Cubic x' y' z' | (x',y',z') <- shrink (x,y,z)]

instance (Arbitrary n, Floating n, Ord n) => Arbitrary (Trail' Line V2 n) where
    arbitrary = lineFromSegments <$> arbitrary
--    shrink (lineSegments -> segs) = lineFromSegments <$> shrink segs

instance (Arbitrary n, Floating n, Ord n) => Arbitrary (Trail' Loop V2 n) where
    arbitrary = closeLine <$> arbitrary
--    shrink (cutLoop -> l) = closeLine <$> shrink l

instance (Arbitrary n, Floating n, Ord n) => Arbitrary (Trail V2 n) where
    arbitrary = QC.oneof [Trail <$> (arbitrary :: Gen (Trail' Loop V2 n)), Trail <$> (arbitrary :: Gen (Trail' Line V2 n))]

