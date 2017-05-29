{-# LANGUAGE FlexibleContexts      #-}


module Diagrams.Test.Transform where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Diagrams.Prelude
import           Diagrams.Direction
import Instances

tests :: TestTree
tests = testGroup "Transform" [

        testProperty "rotating a vector by a number then its additive inverse will yield the original vector" $
          \θ a -> rotate ((θ * (-1)) @@ deg) (rotate ((θ :: Double) @@ deg) (a ::  V2 Double)) =~ a
        , testProperty "under rotated allows scaling along an angle" $
          \θ f a -> under (rotated ((θ :: Double) @@ deg)) (scaleX (f :: Double)) (a ::  V2 Double) =~ (rotate (negated (θ @@ deg)) . (scaleX f) . rotate (θ @@ deg)) a
        , testProperty "a rotation of 0 does nothing" $
          \a -> rotate (0 @@ deg) (a ::  V2 Double) =~ a
        , testProperty "adding 360 degrees to a turn does nothing" $
          \c a -> rotate (((c :: Double) + 360) @@ deg) (a ::  V2 Double) =~ rotate (c @@ deg) a
        , testProperty "over rotated allows scaling along x of a rotated shape" $
          \θ f a -> over (rotated ((θ :: Double) @@ deg)) (scaleX (f :: Double)) (a ::  V2 Double) =~ (rotate (θ @@ deg) . (scaleX f) . rotate (negated (θ @@ deg))) a
        , testProperty "scaleX" $
          \f a b -> (scaleX (f :: Double)) (V2 (a ::  Double) b) =~ V2 (a * f) b
        , testProperty "scaleY" $
          \f a b -> (scaleY (f :: Double)) (V2 (a ::  Double) b) =~ V2 a  (f * b)
        , testProperty "reflectX" $
          \a b -> reflectX (V2 (a ::  Double) b) =~ V2 (a * (-1))  b
        , testProperty "reflectY" $
          \a b -> reflectY (V2 (a ::  Double) b) =~ V2 a  ((-1) * b)
        , testProperty "reflectXY" $
          \a b -> reflectXY (V2 (a ::  Double) b) =~ V2 b a
        , testProperty "translate" $
          \a b c d -> translateX (a :: Double) (translateY b (P (V2 c d ))) =~ P (V2 (a + c) (b + d))
        , testProperty "shear" $
          \a b c d -> shearX (a :: Double) (shearY b (V2 c d)) =~ V2 ((c*b + d) * a + c) (c*b + d)
        , testProperty "(1,0) rotateTo some dir will return normalised dir" $
          \(NonZero a) b -> rotateTo  (dir (V2 (a :: Double) b)) (V2 1 0) =~ signorm (V2 a b)
        , testProperty "rotates" $
          \a c -> rotate ((a :: Double)@@ deg) (c :: V2 Double)   =~ rotate'' ((a :: Double)@@ deg) (c :: V2 Double) && rotate ((a :: Double)@@ deg) (c :: V2 Double)   =~ rotate' ((a :: Double)@@ deg) (c :: V2 Double)
        , testProperty "reflectAbout works for a vector" $
          \a b c d e f -> reflectAbout (P (V2 (a :: Double) b)) (dir (V2 c d)) (V2 e f) =~  over (rotated (atan2A' d c)) reflectY (V2 e f)
        , testProperty "reflectAbout works for a point" $
          \a b c d e f -> reflectAbout (P (V2 (a :: Double) b)) (dir (V2 c d)) (P (V2 e f)) =~ translate (V2 a b)  ((over (rotated (atan2A' d c)) reflectY) ((translate (V2 (-a) (-b)) )  (P (V2 e f))))


        ]

--the original " '' " and a secondary " ' " rotate function for testing

rotation'' :: Floating n => Angle n -> T2 n
rotation'' theta = fromLinear r (linv r)
          where
            r               = rot theta <-> rot (negated theta)
            rot th (V2 x y) = V2 (cosA th * x - sinA th * y)
                                 (sinA th * x + cosA th * y)

rotate'' :: (InSpace V2 n t, Transformable t, Floating n) => Angle n -> t -> t
rotate'' = transform . rotation''

rotation' :: Floating n => Angle n -> T2 n
rotation' theta = fromLinear r (linv r)
            where
            r               = rot theta <-> rot (negated theta)
            rot th (V2 x y) = V2 (c * x - s * y)
                                 (s * x + c * y)
                                  where
                                    c = cosA th
                                    s = sinA th

rotate' :: (InSpace V2 n t, Transformable t, Floating n) => Angle n -> t -> t
rotate'  = transform . rotation'
