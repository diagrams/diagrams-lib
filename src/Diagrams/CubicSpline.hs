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
-- internals of the spline generation algorithm (including in
-- particular a solver for cyclic tridiagonal systems of linear
-- equations), see "Diagrams.CubicSpline.Internal".
--
-----------------------------------------------------------------------------
module Diagrams.CubicSpline
       (
         -- * Constructing paths from cubic splines
         cubicSpline
       ) where

import           Control.Lens                  (view)

import           Diagrams.Core
import           Diagrams.CubicSpline.Internal
import           Diagrams.Located              (Located, at, mapLoc)
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TrailLike            (TrailLike (..))

import           Linear.Affine
import           Linear.Metric

-- | Construct a spline path-like thing of cubic segments from a list of
--   vertices, with the first vertex as the starting point.  The first
--   argument specifies whether the path should be closed.
--
--   <<diagrams/src_Diagrams_CubicSpline_cubicSplineEx.svg#diagram=cubicSplineEx&width=600>>
--
--   > pts = map p2 [(0,0), (2,3), (5,-2), (-4,1), (0,3)]
--   > dot = circle 0.2 # fc blue # lw none
--   > mkPath closed = position (zip pts (repeat dot))
--   >              <> cubicSpline closed pts
--   > cubicSplineEx = (mkPath False ||| strutX 2 ||| mkPath True)
--   >               # centerXY # pad 1.1
--
--   For more information, see <http://mathworld.wolfram.com/CubicSpline.html>.
cubicSpline :: (Vn t ~ v n, TrailLike t, Fractional (v n)) => Bool -> [Point v n] -> t
cubicSpline closed [] = trailLike . closeIf closed $ emptyLine `at` origin
cubicSpline closed ps = flattenBeziers . map f . solveCubicSplineCoefficients closed . map (view lensP) $ ps
  where
    f [a,b,c,d] = [a, (3*a+b)/3, (3*a+2*b+c)/3, a+b+c+d]
    flattenBeziers bs@((b:_):_)
      = trailLike . closeIf closed $ lineFromSegments (map bez bs) `at` P b
    bez [a,b,c,d] = bezier3 (b - a) (c - a) (d - a)

closeIf :: (Metric v, OrderedField n)
        => Bool -> Located (Trail' Line v n) -> Located (Trail v n)
closeIf c = mapLoc (if c then wrapLoop . glueLine else wrapLine)
