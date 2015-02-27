-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.CubicSpline
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A /cubic spline/ is a smooth, connected sequence of cubic curves
-- passing through a given sequence of points.  This module implements
-- a straightforward spline generation algorithm based on solving
-- tridiagonal systems of linear equations.
--
-----------------------------------------------------------------------------
module Diagrams.CubicSpline.Internal
       (
         -- * Solving for spline coefficents
         solveCubicSplineDerivatives
       , solveCubicSplineDerivativesClosed
       , solveCubicSplineCoefficients
       ) where

import           Diagrams.Solve.Tridiagonal

import           Data.List

-- | Use the tri-diagonal solver with the appropriate parameters for an open cubic spline.
solveCubicSplineDerivatives :: Fractional a => [a] -> [a]
solveCubicSplineDerivatives (x:xs) = solveTriDiagonal as bs as ds
  where
    as = replicate (l - 1) 1
    bs = 2 : replicate (l - 2) 4 ++ [2]
    l  = length ds
    ds = zipWith f (xs ++ [last xs]) (x:x:xs)
    f a b = 3*(a - b)

solveCubicSplineDerivatives _ = error "argument to solveCubicSplineDerivatives must be nonempty"

-- | Use the cyclic-tri-diagonal solver with the appropriate parameters for a closed cubic spline.
solveCubicSplineDerivativesClosed :: Fractional a => [a] -> [a]
solveCubicSplineDerivativesClosed xs = solveCyclicTriDiagonal as bs as ds 1 1
  where
    as = replicate (l - 1) 1
    bs = replicate l 4
    l  = length xs
    xs' = cycle xs
    ds = take l $ zipWith f (drop 1 xs') (drop (l - 1) xs')
    f a b = 3*(a - b)

-- | Use the cyclic-tri-diagonal solver with the appropriate parameters for a closed cubic spline.
solveCubicSplineCoefficients :: Fractional a => Bool -> [a] -> [[a]]
solveCubicSplineCoefficients closed xs =
    [ [x,d,3*(x1-x)-2*d-d1,2*(x-x1)+d+d1]
    | (x,x1,d,d1) <- zip4 xs' (tail xs') ds' (tail ds')
    ]
  where
    ds | closed    = solveCubicSplineDerivativesClosed xs
       | otherwise = solveCubicSplineDerivatives xs
    close as | closed    = as ++ [head as]
             | otherwise = as
    xs' = close xs
    ds' = close ds
