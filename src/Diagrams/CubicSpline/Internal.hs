{-# LANGUAGE GADTs #-}

-- |
-- Module      :  Diagrams.CubicSpline.Internal
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A /cubic spline/ is a smooth, connected sequence of cubic curves
-- passing through a given sequence of points.  This module implements
-- a straightforward spline generation algorithm based on solving
-- tridiagonal systems of linear equations.
module Diagrams.CubicSpline.Internal (
  -- * Solving for spline coefficents
  solveCubicSplineDerivatives,
  solveCubicSplineDerivativesClosed,
  solveCubicSplineCoefficients,
) where

import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty.Compat as NE
import Diagrams.CubicSpline.NonSingleton (NonSingleton (..))
import qualified Diagrams.CubicSpline.NonSingleton as NS
import Diagrams.Solve.Tridiagonal

-- | Use the tri-diagonal solver with the appropriate parameters for an open cubic spline.
--
--   See e.g. https://observablehq.com/@jrus/cubic-spline
solveCubicSplineDerivatives :: Fractional a => NonSingleton a -> NonEmpty a
solveCubicSplineDerivatives xs = solveTriDiagonal as bs as (NS.toNonEmpty ds)
 where
  as = NE.map (const 1) (NS.tail ds)
  bs = 2 :| map (const 4) (NE.tail (NS.tail ds)) ++ [2]
  -- zipWith f [x1, x2, x3, ..., xn, xn] [x0, x0, x1, ... x{n-2}, x{n-1}]
  ds = NS.zipWith f (NS.dupLast (NS.tail xs)) (NS.dupFirst xs)
  f a b = 3 * (a - b)

-- | Use the cyclic-tri-diagonal solver with the appropriate parameters for a closed cubic spline.
solveCubicSplineDerivativesClosed :: Fractional a => NonSingleton a -> NonEmpty a
solveCubicSplineDerivativesClosed xs = solveCyclicTriDiagonal as bs as ds 1 1
 where
  as = fmap (const 1) (NS.tail xs)
  bs = fmap (const 4) (NS.toNonEmpty xs)

  -- zipWith f [x1, x2, x3, ..., xn, x0] [xn, x0, x1, ..., x{n-1}]
  -- ds = take l $ zipWith f (drop 1 xs') (drop (l - 1) xs') where l = length xs
  xsNE = NS.toNonEmpty xs
  ds = NE.zipWith f (NE.tail xsNE |: NE.head xsNE) (NE.cons (NE.last xsNE) xsNE)
  f a b = 3 * (a - b)

  (|:) :: [a] -> a -> NonEmpty a
  (|:) ys y = foldr NE.cons (NE.singleton y) ys

-- | Use the cyclic-tri-diagonal solver with the appropriate parameters for a closed cubic spline.
solveCubicSplineCoefficients :: Fractional a => Bool -> NonSingleton a -> [[a]]
solveCubicSplineCoefficients closed xs =
  [ [x, d, 3 * (x1 - x) - 2 * d - d1, 2 * (x - x1) + d + d1]
  | (x, x1, d, d1) <- zip4 (NE.toList xs') (NE.tail xs') (NE.toList ds') (NE.tail ds')
  ]
 where
  ds
    | closed = solveCubicSplineDerivativesClosed xs
    | otherwise = solveCubicSplineDerivatives xs
  close as
    | closed = as <> NE.singleton (NE.head as)
    | otherwise = as
  xs' = close (NS.toNonEmpty xs)
  ds' = close ds
