{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Quickcheck where

import           Control.Lens              hiding (at, from, ( # ))
import           Numeric.Extras

import           Diagrams.Coordinates
import           Diagrams.Located
import           Diagrams.Prelude
import           Diagrams.Solve.Polynomial
import           Diagrams.Trail            (isLine, linePoints)

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Control.DeepSeq

------------------------------------------------------------
    -- Approximate Comparison for Doubles, Points

epsilon = 0.001

class Approx a where
  (=~) :: a -> a -> Bool

infix 4 =~

--instance (Fractional a, Ord a) => Approx a where
instance Approx Double where
  (=~) a b = abs (a - b) < epsilon

instance Approx n => Approx (V2 n) where
    z1 =~ z2 = (z1^._x) =~ (z2^._x) && (z1^._y) =~ (z2^._y)

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

instance Arbitrary (v n) => Arbitrary (Point v n) where
    arbitrary = P <$> arbitrary

instance Arbitrary n => Arbitrary (Angle n) where
    arbitrary = review rad <$> arbitrary

instance (Arbitrary n, Floating n) => Arbitrary (Direction V2 n) where
    arbitrary = rotate <$> arbitrary <*> pure xDir

-- -- | Not a valid Show instance because not valid Haskell input
-- instance (Show n, RealFloat n) => Show (Direction V2 n) where
--     show d = "Dir" <> ( show $ d ^. _theta . turn )

instance (Arbitrary a, Arbitrary (Vn a)) => Arbitrary (Located a) where
    arbitrary = at <$> arbitrary <*> arbitrary

instance Arbitrary n => Arbitrary (Offset Closed V2 n) where
    arbitrary = OffsetClosed <$> arbitrary

instance Arbitrary n =>  Arbitrary (Segment Closed V2 n) where
    arbitrary = oneof [Linear <$> arbitrary, Cubic <$> arbitrary <*> arbitrary <*> arbitrary]

instance (Arbitrary n, Floating n, Ord n) => Arbitrary (Trail' Line V2 n) where
    arbitrary = lineFromSegments <$> arbitrary

instance (Arbitrary n, Floating n, Ord n) => Arbitrary (Trail' Loop V2 n) where
    arbitrary = closeLine <$> arbitrary

instance (Arbitrary n, Floating n, Ord n) => Arbitrary (Trail V2 n) where
    arbitrary = oneof [Trail <$> (arbitrary :: Gen (Trail' Loop V2 n)), Trail <$> (arbitrary :: Gen (Trail' Line V2 n))]

------------------------------------------------------------
-- Some quickcheck tests to start with

tests = testGroup "Properties"
         [
    testGroup "TwoD.Arc" [
           testProperty "arc start point is at radius 1 in the starting direction" $ \d a ->
               pathVertices (arc d a :: Path V2 Double) ^? _head . _head =~ Just (origin .+^ fromDirection d )
           , testProperty "arc end point is at radius 1 in the ending direction" $ \d a ->
               pathVertices (arc d a :: Path V2 Double) ^? _head . _last =~ Just (origin .+^ fromDirection (rotate a d))
         ]

    , testGroup "TwoD.Types" [
         testProperty "R2 vector addition is commutative" $
           \u v -> (u :: V2 Double) ^+^ v =~ v ^+^ u
         , testProperty "R2 subtraction is the inverse of addition" $
           \u v -> u ^+^ v ^-^ v =~ (u :: V2 Double)
         , testProperty "R2 vector negation squared is identity" $
           \u -> negated (negated (u :: V2 Double)) =~ u
         ]
    , testGroup "Angle" [
         testProperty "2π radians per turn" $
           \θ -> θ^.rad =~ θ^.turn*2*(pi :: Double)
         , testProperty "360 degrees per turn" $
           \θ -> θ^.deg =~ θ^.turn*(360 :: Double)
         , testProperty "Angle vector addition is commutative" $
           \θ φ -> (θ :: Angle Double) ^+^ φ =~ φ ^+^ θ
         , testProperty "Angle subtraction is the inverse of addition" $
           \θ φ -> (θ :: Angle Double) ^+^ φ ^-^ φ =~ θ
         , testProperty "Angle vector negation squared is identity" $
           \θ -> negated (negated (θ :: Angle Double)) =~ θ
         ]
    , testGroup "Solve" [
         testProperty "solutions found satisfy quadratic equation" $
         \a b c -> let sat x =  a * x * x + b * x + c =~ (0 :: Double) in all sat (quadForm a b c)
-- could verify number of solutions, but we would just duplicate the function definition
        , testProperty "solutions found satisfy cubic equation" $
         \a b c d -> let sat x =  a * x * x * x + b * x * x + c * x + d =~ (0 :: Double) in all sat (cubForm a b c d)
        ]
    , testGroup "cubicSpline" [
         testProperty "Open cubic spline interpolates all points" $
         \pts -> length pts > 1 ==> and (zipWith (=~) pts (cubicSpline False pts :: [P2 Double]))
         , testProperty "Closed cubic spline interpolates all points" $
           \pts -> length pts > 1 ==> and (zipWith (=~) pts (cubicSpline True pts :: [P2 Double]))
    ]
      , testGroup "Trail" [
         testProperty "glueLine . cutLoop === id" $
         \l -> glueLine (cutLoop l :: Trail' Line V2 Double) =~ l
         , testProperty "cutLoop ends at starting point" $
           \l -> let ps = linePoints (cutLoop (l :: Trail' Loop V2 Double) `at` origin) in (ps ^? _head) =~ (ps ^? _last)
         , testProperty "cutTrail makes a Line" $
           \t -> isLine (cutTrail (t :: Trail V2 Double))
          , testProperty "fromSegments . lineSegments === id" $
            \l -> fromSegments (lineSegments l) =~ (l :: Trail' Line V2 Double)
          , testProperty "lineSegments . fromSegments === id" $
            \segs -> lineSegments (fromSegments segs) =~ (segs :: [Segment Closed V2 Double])
    ]]
