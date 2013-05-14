{-# LANGUAGE FlexibleContexts #-}
module DiamBench where
import Criterion.Main
import Diagrams.Prelude
import Diagrams.ThreeD.Shapes
import Diagrams.ThreeD.Types

-- Comparing the performance of two different diameter implementations

-- Old definition:
diameter1 :: Enveloped a => V a -> a -> Scalar (V a)
diameter1 v a = magnitude (envelopeV v a ^-^ envelopeV (negateV v) a)

-- New definition:
diameter2 :: Enveloped a => V a -> a -> Scalar (V a)
diameter2 v a = case appEnvelope $ getEnvelope a of
  (Just env) -> (env v + env (negateV v)) * magnitude v
  Nothing -> 0

--test :: (VectorSpace v, Fractional (Scalar v), Enum (Scalar v)) => (v -> t) -> v -> [t]
test f v = [ f (v ^* d) | d <- [0.0, 5.0 .. 100.0] ]

main = defaultMain
   [ bench "diameter1" $ nf (\d -> test (`diameter1` d) $ 1 & 3) (unitSquare :: D R2)
   , bench "diameter2" $ nf (\d -> test (`diameter2` d) $ 1 & 3) (unitSquare :: D R2)
   , bench "diameter3" $ nf (\d -> test (`diameter1` d) $ (-1) & 1 & 3) (sphere :: D R3)
   , bench "diameter4" $ nf (\d -> test (`diameter2` d) $ (-1) & 1 & 3) (sphere :: D R3)
   ]


{- mgsloan's results

warming up
estimating clock resolution...
mean is 1.277677 us (640001 iterations)
found 3123 outliers among 639999 samples (0.5%)
  2933 (0.5%) high severe
estimating cost of a clock call...
mean is 28.94072 ns (7 iterations)
found 1 outliers among 7 samples (14.3%)
  1 (14.3%) high severe

benchmarking diameter1
mean: 41.10492 us, lb 41.02676 us, ub 41.20513 us, ci 0.950
std dev: 453.7027 ns, lb 370.9014 ns, ub 583.8290 ns, ci 0.950

benchmarking diameter2
mean: 40.12666 us, lb 40.04038 us, ub 40.23655 us, ci 0.950
std dev: 498.8693 ns, lb 415.3504 ns, ub 637.7434 ns, ci 0.950

benchmarking diameter3
mean: 1.762319 us, lb 1.759122 us, ub 1.765858 us, ci 0.950
std dev: 17.29888 ns, lb 15.30558 ns, ub 20.23129 ns, ci 0.950

benchmarking diameter4
mean: 1.692065 us, lb 1.687748 us, ub 1.698442 us, ci 0.950
std dev: 26.41623 ns, lb 20.34119 ns, ub 40.98644 ns, ci 0.950
found 4 outliers among 100 samples (4.0%)
  3 (3.0%) high mild
  1 (1.0%) high severe
variance introduced by outliers: 8.481%
variance is slightly inflated by outliers
-}
