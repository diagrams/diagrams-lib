{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Control.Applicative
import           Data.Default
import           Data.Ratio
import           Data.VectorSpace
import           Test.QuickCheck

import           Diagrams.Prelude


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

-- The following tests don't work very well since they fail on cases
-- where the answer is not quite within the stated tolerance.  But
-- they have still been useful in finding cases where the adjustment
-- methods fail for other reasons.  Basically, (1) run QC (2) get
-- counterexample (3) check counterexample by hand to see whether it
-- is close.
--
-- Unfortunately we can't use Q2 since arc-length-related functions
-- ultimately work by finding the magnitude vectors, which uses sqrt.

prop_adjustSeg_toAbs_len :: Segment R2 -> Scalar R2 -> AdjustSide -> Property
prop_adjustSeg_toAbs_len s len side = abs len > eps ==>
  arcLength (adjustSegment s with { adjMethod = ToAbsolute len, adjSide = side }) eps ==~ abs len

eps = 1/10^10
x ==~ y = abs (x - y) < eps

prop_adjustSeg_byAbs_len :: Segment R2 -> Scalar R2 -> AdjustSide -> Bool
prop_adjustSeg_byAbs_len s len side =
  arcLength (adjustSegment s with { adjMethod = ByAbsolute len, adjSide = side }) eps ==~ abs (arcLength s eps + len)
