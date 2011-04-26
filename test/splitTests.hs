{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, TypeFamilies #-}

import Test.QuickCheck
import Control.Applicative
import Data.Ratio
import Data.VectorSpace

import Diagrams.Segment


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

