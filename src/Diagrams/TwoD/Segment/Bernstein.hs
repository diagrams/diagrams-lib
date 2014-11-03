{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies  #-}

module Diagrams.TwoD.Segment.Bernstein
  ( BernsteinPoly (..)
	, listToBernstein
	, evaluateBernstein

	, degreeElevate
	, bernsteinDeriv
	, evaluateBernsteinDerivs
	) where

import           Data.List           (tails)
import           Diagrams.Core.V
import           Diagrams.Parametric
import           Linear.V1

-- find the binomial coefficients of degree n.
binomials :: Num n => Int -> [n]
binomials n = map fromIntegral $ scanl (\x m -> x * (n - m+1) `quot` m) 1 [1..n]

data BernsteinPoly n = BernsteinPoly
  { bernsteinDegree :: Int
  , bernsteinCoeffs :: [n]
  } deriving (Show, Functor)

type instance V        (BernsteinPoly n) = V1
type instance N        (BernsteinPoly n) = n
type instance Codomain (BernsteinPoly n) = V1

-- | Create a bernstein polynomial from a list of coëfficients.
listToBernstein :: Fractional n => [n] -> BernsteinPoly n
listToBernstein [] = 0
listToBernstein l  = BernsteinPoly (length l - 1) l

-- | Degree elevate a bernstein polynomial a number of times.
degreeElevate :: Fractional n => BernsteinPoly n -> Int -> BernsteinPoly n
degreeElevate b                    0     = b
degreeElevate (BernsteinPoly lp p) times =
  degreeElevate (BernsteinPoly (lp+1) (head p:inner p 1)) (times-1)
  where
    n = fromIntegral lp

    inner []         _ = [0]
    inner [a]        _ = [a]
    inner (a:b:rest) i = (i*a/(n+1) + b*(1 - i/(n+1))) : inner (b:rest) (i+1)

-- | Evaluate the bernstein polynomial.
evaluateBernstein :: Fractional n => BernsteinPoly n -> n -> n
evaluateBernstein (BernsteinPoly _ [])       _ = 0
evaluateBernstein (BernsteinPoly _ [b])      _ = b
evaluateBernstein (BernsteinPoly lp (b':bs)) t = go t n (b'*u) 2 bs
  where
    u = 1-t
    n = fromIntegral lp

    go tn bc tmp _ [b]      = tmp + tn*bc*b
    go tn bc tmp i (b:rest) =
      go (tn*t)              -- tn
         (bc*(n - i+1)/i)    -- bc
         ((tmp + tn*bc*b)*u) -- tmp
         (i+1)               -- i
         rest
    go _ _ _ _ []           = error "evaluateBernstein: impossible"

-- | Evaluate the bernstein polynomial and its derivatives.
evaluateBernsteinDerivs :: Fractional n => BernsteinPoly n -> n -> [n]
evaluateBernsteinDerivs b t
  | bernsteinDegree b == 0 = [evaluateBernstein b t]
  | otherwise              = evaluateBernstein b t : evaluateBernsteinDerivs (bernsteinDeriv b) t

-- | Find the derivative of a bernstein polynomial.
bernsteinDeriv :: Fractional n => BernsteinPoly n -> BernsteinPoly n
bernsteinDeriv (BernsteinPoly 0 _)  = 0
bernsteinDeriv (BernsteinPoly lp p) =
  -- BernsteinPoly (lp-1) $ map (* fromIntegral lp) $ zipWith (-) (tail p) p
  BernsteinPoly (lp-1) $ zipWith (\a b -> (a - b) * fromIntegral lp) (tail p) p

instance Fractional n => Parametric (BernsteinPoly n) where
	atParam b = V1 . evaluateBernstein b
instance Num n        => DomainBounds (BernsteinPoly n)
instance Fractional n => EndValues    (BernsteinPoly n)
instance Fractional n => Sectionable  (BernsteinPoly n) where
	splitAtParam  = bernsteinSplit
	reverseDomain (BernsteinPoly i xs) = BernsteinPoly i (reverse xs)

-- | Split a bernstein polynomial
bernsteinSplit :: Num n => BernsteinPoly n -> n -> (BernsteinPoly n, BernsteinPoly n)
bernsteinSplit (BernsteinPoly lp p) t =
  (BernsteinPoly lp $ map head controls,
   BernsteinPoly lp $ reverse $ map last controls)
  where
    interp a b = (1-t)*a + t*b

    terp [_] = []
    terp l   = let ctrs = zipWith interp l (tail l)
               in  ctrs : terp ctrs
    controls = p : terp p

instance Fractional n => Num (BernsteinPoly n) where
  ba@(BernsteinPoly la a) + bb@(BernsteinPoly lb b)
    | la < lb   = BernsteinPoly lb $ zipWith (+) (bernsteinCoeffs $ degreeElevate ba $ lb - la) b
    | la > lb   = BernsteinPoly la $ zipWith (+) a (bernsteinCoeffs $ degreeElevate bb $ la - lb)
    | otherwise = BernsteinPoly la $ zipWith (+) a b

  ba@(BernsteinPoly la a) - bb@(BernsteinPoly lb b)
    | la < lb   = BernsteinPoly lb $ zipWith (-) (bernsteinCoeffs $ degreeElevate ba (lb - la)) b
    | la > lb   = BernsteinPoly la $ zipWith (-) a (bernsteinCoeffs $ degreeElevate bb (la - lb))
    | otherwise = BernsteinPoly la $ zipWith (-) a b

  (BernsteinPoly la a) * (BernsteinPoly lb b) =
    BernsteinPoly (la+lb) $
    zipWith (flip (/)) (binomials (la + lb)) $
                   init $ map sum $
                   map (zipWith (*) a') (down b') ++
									 map (zipWith (*) (reverse b')) (tail $ tails a')
                   -- zipWith (zipWith (*)) (tail $ tails a') (repeat $ reverse b')
    where down l = tail $ scanl (flip (:)) [] l -- [[1], [2, 1], [3, 2, 1], ...
          a' = zipWith (*) a (binomials la)
          b' = zipWith (*) b (binomials lb)

  fromInteger a = BernsteinPoly 0 [fromInteger a]

  signum (BernsteinPoly _ [])    = 0
  signum (BernsteinPoly _ (a:_)) = BernsteinPoly 0 [signum a]

  abs = fmap abs


