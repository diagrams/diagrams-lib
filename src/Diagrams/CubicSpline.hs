{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.CubicSpline
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A /cubic spline/ is a smooth, connected sequence of cubic curves
-- passing through a given sequence of points.  This module provides
-- the 'cubicSpline' method, which can be used to create closed or
-- open cubic splines from a list of points.  For access to the
-- internals of the spline generation algorithm, see
-- "Diagrams.CubicSpline.Internal".
--
-----------------------------------------------------------------------------
module Diagrams.CubicSpline
       (
         -- * Constructing paths from cubic splines
         cubicSpline
       ) where

import           Diagrams.Core
import           Diagrams.Core.Points
import           Diagrams.CubicSpline.Internal
import           Diagrams.Located              (at)
import           Diagrams.Located
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TrailLike            (TrailLike (..))

-- for e.g. the Fractional (Double, Double) instance
import           Data.NumInstances.Tuple       ()

import           Control.Newtype
import           Data.Semigroup
import           Data.VectorSpace

-- | Construct a spline path-like thing of cubic segments from a list of
--   vertices, with the first vertex as the starting point.  The first
--   argument specifies whether the path should be closed.
--   See: <http://mathworld.wolfram.com/CubicSpline.html>
cubicSpline :: (TrailLike t, Fractional (V t)) => Bool -> [Point (V t)] -> t
cubicSpline c [] = trailLike . closeIf c $ emptyLine `at` origin
cubicSpline c ps = flattenBeziers . map f . solveCubicSplineCoefficients c . map unpack $ ps
  where
    f [a,b,c,d] = [a, (3*a+b)/3, (3*a+2*b+c)/3, a+b+c+d]
    flattenBeziers bs@((b:_):_)
      = trailLike . closeIf c $ lineFromSegments (map bez bs) `at` P b
    bez [a,b,c,d] = bezier3 (b - a) (c - a) (d - a)

closeIf :: (InnerSpace v, OrderedField (Scalar v))
        => Bool -> Located (Trail' Line v) -> Located (Trail v)
closeIf c = mapLoc (if c then wrapLoop . glueLine else wrapLine)
