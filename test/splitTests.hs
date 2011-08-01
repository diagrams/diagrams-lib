{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, TypeFamilies #-}

import Test.QuickCheck
import Control.Applicative
import Data.Ratio
import Data.VectorSpace
import Data.Default

import Diagrams.Prelude


type Q2 = (Rational, Rational)


instance AdditiveGroup (Rational) where { zeroV=0; (^+^) = (+); negateV = negate }

instance VectorSpace (Rational) where
  type Scalar (Rational) = Rational
  (*^) = (*)

instance (VectorSpace v, Arbitrary v) => Arbitrary (Segment v) where
   arbitrary = oneof [Linear <$> arbitrary, Cubic <$> arbitrary <*> arbitrary <*> arbitrary]



prop_paramSplit :: Segment Q2 -> Scalar Q2 -> Scalar Q2 -> Bool
prop_paramSplit s t u
  | u < t     = atParam s u == atParam l (u / t)
  | otherwise = atParam s u == atParam s t ^+^ atParam r ((u - t) / (1.0 - t))
  where (l,r) = splitAtParam s t

prop_adjustSegParams :: Segment Q2 -> Scalar Q2 -> Scalar Q2 -> Scalar Q2 -> Property
prop_adjustSegParams s p1 p2 t = p1 /= p2 ==>
    atParam s t == atParam s p1 ^+^ atParam s' ((t - p1) / (p2 - p1))
  where s' = adjustSegmentToParams s p1 p2

instance Arbitrary AdjustSide where
  arbitrary = elements [Start, End, Both]

prop_adjustSeg_toAbs_len :: Segment R2 -> Scalar R2 -> AdjustSide -> Property
prop_adjustSeg_toAbs_len s len side = len > eps ==>
  arcLength (adjustSegment s with { adjMethod = ToAbsolute len, adjSide = side }) eps ==~ len

eps = 1/10^10
x ==~ y = abs (x - y) < eps