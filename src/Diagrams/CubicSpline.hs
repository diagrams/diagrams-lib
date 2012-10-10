{-# LANGUAGE TypeFamilies
           , FlexibleContexts
  #-}
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

import Diagrams.CubicSpline.Internal

import Diagrams.Core
import Diagrams.Core.Points

import Diagrams.Segment
import Diagrams.Path

import Data.NumInstances ()   -- for e.g. the Fractional (Double, Double) instance

import Control.Newtype
import Data.Semigroup

-- | Construct a spline path-like thing of cubic segments from a list of
--   vertices, with the first vertex as the starting point.  The first
--   argument specifies whether the path should be closed.
--   See: <http://mathworld.wolfram.com/CubicSpline.html>
cubicSpline :: (PathLike p, Fractional (V p)) => Bool -> [Point (V p)] -> p
cubicSpline _      [] = mempty
cubicSpline closed ps = flattenBeziers . map f . solveCubicSplineCoefficients closed . map unpack $ ps
  where
    f [a,b,c,d] = [a, (3*a+b)/3, (3*a+2*b+c)/3, a+b+c+d]
    flattenBeziers bs@((b:_):_) = pathLike (P b) False (map bez bs)
    bez [a,b,c,d] = bezier3 (b - a) (c - a) (d - a)
