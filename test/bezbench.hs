{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, TypeFamilies, NoMonomorphismRestriction #-}
import Data.VectorSpace
import Data.NumInstances
import Criterion.Main
import Diagrams.Segment

type Q2 = (Rational,Rational)
type R2 = (Double,Double)

instance AdditiveGroup (Rational) where { zeroV=0; (^+^) = (+); negateV = negate }

instance VectorSpace (Rational) where
  type Scalar (Rational) = Rational
  (*^) = (*)

b = Cubic (0,1) (1,1) (1,0)
test f = [ f t | t <- [0.0,0.01..1.0] ]

atParam' c@(Cubic _ _ _) t = x
    where ((Cubic _ _ x),_) = splitAtParam c t

main = defaultMain
   [ bench "atParam  R2" $ nf (\b -> test $ atParam  b) (b :: Segment R2)
   , bench "atParam' R2" $ nf (\b -> test $ atParam' b) (b :: Segment R2)
   , bench "atParam  Q2" $ nf (\b -> test $ atParam  b) (b :: Segment Q2)
   , bench "atParam' Q2" $ nf (\b -> test $ atParam' b) (b :: Segment Q2)
   ]
