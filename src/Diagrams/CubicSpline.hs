{-# LANGUAGE TypeFamilies
           , FlexibleContexts
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.CubicSpline
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Generic functionality for constructing cubic splines
--
-----------------------------------------------------------------------------
module Diagrams.CubicSpline
       (
         -- * Constructing paths from cubic splines
         cubicSpline
       ) where

import Diagrams.CubicSpline.Internal

import Graphics.Rendering.Diagrams
import Diagrams.Segment
import Diagrams.Path
import Diagrams.Util

import Control.Newtype
import Data.Monoid
import Data.List
import Data.VectorSpace
import Data.NumInstances

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
