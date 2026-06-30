{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Diagrams.TwoD.Segment.Bernstein
-- Copyright   :  (c) 2014-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Bernstein polynomials, used internally by code to find
-- intersections of paths.  This module is probably not of any
-- relevance to most users of diagrams.
module Diagrams.TwoD.Segment.Bernstein (
  BernsteinPoly (..),
  listToBernstein,
  evaluateBernstein,
  degreeElevate,
  bernsteinDeriv,
  evaluateBernsteinDerivs,
) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty.Compat as NE
import Diagrams.Core.V
import Diagrams.Parametric
import Linear.V1

-- | Compute the binomial coefficients of degree n.
binomials :: Num n => Int -> NonEmpty n
binomials n = NE.map fromIntegral $ NE.scanl (\x m -> x * (n - m + 1) `quot` m) 1 [1 .. n]

data BernsteinPoly n = BernsteinPoly
  { bernsteinDegree :: Int
  , bernsteinCoeffs :: NonEmpty n
  }
  deriving (Show, Functor)

type instance V (BernsteinPoly n) = V1
type instance N (BernsteinPoly n) = n
type instance Codomain (BernsteinPoly n) = V1

-- | Create a bernstein polynomial from a list of coëfficients.
listToBernstein :: Fractional n => [n] -> BernsteinPoly n
listToBernstein l = case NE.nonEmpty l of
  Nothing -> 0
  Just ne -> BernsteinPoly (length l - 1) ne

-- | Degree elevate a bernstein polynomial a number of times.
degreeElevate :: Fractional n => BernsteinPoly n -> Int -> BernsteinPoly n
degreeElevate b 0 = b
degreeElevate (BernsteinPoly lp p) times =
  degreeElevate (BernsteinPoly (lp + 1) (NE.head p :| inner p 1)) (times - 1)
 where
  n = fromIntegral lp

  inner (a :| []) _ = [a]
  inner (a :| b : rest) i = (i * a / (n + 1) + b * (1 - i / (n + 1))) : inner (b :| rest) (i + 1)

-- | Evaluate the bernstein polynomial.
evaluateBernstein :: Fractional n => BernsteinPoly n -> n -> n
evaluateBernstein (BernsteinPoly _ (b :| [])) _ = b
evaluateBernstein (BernsteinPoly lp (b' :| bs)) t = go t n (b' * u) 2 bs
 where
  u = 1 - t
  n = fromIntegral lp

  go tn bc tmp _ [b] = tmp + tn * bc * b
  go tn bc tmp i (b : rest) =
    go
      (tn * t) -- tn
      (bc * (n - i + 1) / i) -- bc
      ((tmp + tn * bc * b) * u) -- tmp
      (i + 1) -- i
      rest
  go _ _ _ _ [] = error "evaluateBernstein: impossible"

-- | Evaluate the bernstein polynomial and its derivatives.
evaluateBernsteinDerivs :: Fractional n => BernsteinPoly n -> n -> [n]
evaluateBernsteinDerivs b t
  | bernsteinDegree b == 0 = [evaluateBernstein b t]
  | otherwise = evaluateBernstein b t : evaluateBernsteinDerivs (bernsteinDeriv b) t

-- | Find the derivative of a bernstein polynomial.
bernsteinDeriv :: Fractional n => BernsteinPoly n -> BernsteinPoly n
bernsteinDeriv (BernsteinPoly _ (_ :| [])) = 0
bernsteinDeriv (BernsteinPoly lp p@(_ :| (a1 : as))) =
  -- BernsteinPoly (lp-1) $ map (* fromIntegral lp) $ zipWith (-) (drop 1 p) p
  BernsteinPoly (lp - 1) $ NE.zipWith (\a b -> (a - b) * fromIntegral lp) (a1 :| as) p

instance Fractional n => Parametric (BernsteinPoly n) where
  atParam b = V1 . evaluateBernstein b
instance Num n => DomainBounds (BernsteinPoly n)
instance Fractional n => EndValues (BernsteinPoly n)
instance Fractional n => Sectionable (BernsteinPoly n) where
  splitAtParam = bernsteinSplit
  reverseDomain (BernsteinPoly i xs) = BernsteinPoly i (NE.reverse xs)

-- | Split a bernstein polynomial.
bernsteinSplit :: Num n => BernsteinPoly n -> n -> (BernsteinPoly n, BernsteinPoly n)
bernsteinSplit (BernsteinPoly lp p) t =
  ( BernsteinPoly lp $ NE.map NE.head controls
  , BernsteinPoly lp $ NE.reverse $ NE.map NE.last controls
  )
 where
  interp a b = (1 - t) * a + t * b

  -- terp :: NonEmpty n -> [NonEmpty n]
  terp l@(_ :| as) = case NE.nonEmpty as of
    Nothing -> []
    Just as' ->
      let ctrs = NE.zipWith interp l as'
       in ctrs : terp ctrs
  controls = p :| terp p

instance Fractional n => Num (BernsteinPoly n) where
  ba@(BernsteinPoly la a) + bb@(BernsteinPoly lb b)
    | la < lb = BernsteinPoly lb $ NE.zipWith (+) (bernsteinCoeffs $ degreeElevate ba $ lb - la) b
    | la > lb = BernsteinPoly la $ NE.zipWith (+) a (bernsteinCoeffs $ degreeElevate bb $ la - lb)
    | otherwise = BernsteinPoly la $ NE.zipWith (+) a b

  ba@(BernsteinPoly la a) - bb@(BernsteinPoly lb b)
    | la < lb = BernsteinPoly lb $ NE.zipWith (-) (bernsteinCoeffs $ degreeElevate ba (lb - la)) b
    | la > lb = BernsteinPoly la $ NE.zipWith (-) a (bernsteinCoeffs $ degreeElevate bb (la - lb))
    | otherwise = BernsteinPoly la $ NE.zipWith (-) a b

  (BernsteinPoly la a) * (BernsteinPoly lb b) =
    BernsteinPoly (la + lb) $
      NE.zipWith (flip (/)) (binomials (la + lb)) $
        NE.map sum $
          NE.appendList
            (NE.map (NE.zipWith (*) a') (down b'))
            (map (NE.zipWith (*) (NE.reverse b')) (NE.tail $ NE.tails1 a'))
   where
    down (x :| xs) = NE.scanl (flip NE.cons) (x :| []) xs -- [[1], [2, 1], [3, 2, 1], ...
    a' = NE.zipWith (*) a (binomials la)
    b' = NE.zipWith (*) b (binomials lb)

  fromInteger a = BernsteinPoly 0 (fromInteger a :| [])

  signum (BernsteinPoly _ (a :| _)) = BernsteinPoly 0 (signum a :| [])

  abs = fmap abs
