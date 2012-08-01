{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, TypeFamilies, NoMonomorphismRestriction #-}
import Criterion.Main
import Diagrams.Prelude

-- Comparing the performance of two different diameter implementations

-- Current definition:
diameter1 :: Enveloped a => V a -> a -> Scalar (V a)
diameter1 v a = magnitude (envelopeV v a ^-^ envelopeV (negateV v) a)

-- Possibly faster definition (not according to these tests):
diameter2 :: Enveloped a => V a -> a -> Scalar (V a)
diameter2 v = maybe 0 (\env -> env u + env (negateV u)) . appEnvelope . getEnvelope
 where
  u = normalized v

test f = [ f (rotate (Deg d) unitX) | d <- [0.0, 5.0 .. 360.0] ]

main = defaultMain
   [ bench "diameter1" $ nf (\d -> test (`diameter1` d)) (unitSquare :: D R2)
   , bench "diameter2" $ nf (\d -> test (`diameter2` d)) (unitSquare :: D R2)
   ]
