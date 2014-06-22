{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
         solveTriDiagonal
       , solveCyclicTriDiagonal
       , solveCubicSplineDerivatives
       , solveCubicSplineDerivativesClosed
       , solveCubicSplineCoefficients
       ) where

import           Data.List

-- | Solves a system of the form 'A*X=D' for 'x' where 'A' is an
--   'n' by 'n' matrix with 'bs' as the main diagonal and
--   'as' the diagonal below and 'cs' the diagonal above.
--   See: <http://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm>
solveTriDiagonal :: Fractional a => [a] -> [a] -> [a] -> [a] -> [a]
solveTriDiagonal as (b0:bs) (c0:cs) (d0:ds) = h cs' ds'
  where
    cs' = c0 / b0 : f cs' as bs cs
    f _ [_] _ _ = []
    f (c':cs') (a:as) (b:bs) (c:cs) = c / (b - c' * a) : f cs' as bs cs
    f _ _ _ _ = error "solveTriDiagonal.f: impossible!"

    ds' = d0 / b0 : g ds' as bs cs' ds
    g _ [] _ _ _ = []
    g (d':ds') (a:as) (b:bs) (c':cs') (d:ds) = (d - d' * a)/(b - c' * a) : g ds' as bs cs' ds
    g _ _ _ _ _ = error "solveTriDiagonal.g: impossible!"

    h _ [d] = [d]
    h (c:cs) (d:ds) = let xs@(x:_) = h cs ds in d - c * x : xs
    h _ _ = error "solveTriDiagonal.h: impossible!"

solveTriDiagonal _ _ _ _ = error "arguments 2,3,4 to solveTriDiagonal must be nonempty"

-- Helper that applies the passed function only to the last element of a list
modifyLast :: (a -> a) -> [a] -> [a]
modifyLast _ []     = []
modifyLast f [a]    = [f a]
modifyLast f (a:as) = a : modifyLast f as

-- Helper that builds a list of length n of the form: '[s,m,m,...,m,m,e]'
sparseVector :: Int -> a -> a -> a -> [a]
sparseVector n s m e
    | n < 1     = []
    | otherwise = s : h (n - 1)
  where
    h 1 = [e]
    h n = m : h (n - 1)

-- | Solves a system similar to the tri-diagonal system using a special case
--   of the Sherman-Morrison formula <http://en.wikipedia.org/wiki/Sherman-Morrison_formula>.
--   This code is based on /Numerical Recpies in C/'s @cyclic@ function in section 2.7.
solveCyclicTriDiagonal :: Fractional a => [a] -> [a] -> [a] -> [a] -> a -> a -> [a]
solveCyclicTriDiagonal as (b0:bs) cs ds alpha beta = zipWith ((+) . (fact *)) zs xs
  where
    l = length ds
    gamma = -b0
    us = sparseVector l gamma 0 alpha

    bs' = (b0 - gamma) : modifyLast (subtract (alpha*beta/gamma)) bs

    xs@(x:_) = solveTriDiagonal as bs' cs ds
    zs@(z:_) = solveTriDiagonal as bs' cs us

    fact = -(x + beta * last xs / gamma) / (1.0 + z + beta * last zs / gamma)

solveCyclicTriDiagonal _ _ _ _ _ _ = error "second argument to solveCyclicTriDiagonal must be nonempty"

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
