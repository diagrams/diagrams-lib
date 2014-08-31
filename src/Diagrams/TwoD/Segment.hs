{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Orphan Traced instances for Segment Closed R2 and FixedSegment R2.
-- They can't go in Traced; but they shouldn't really go in
-- Diagrams.Segment either because we only have Traced instances for
-- the special case of R2.
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Segment
-- Copyright   :  (c) 2012 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Segments in two dimensions are special since we may meaningfully
-- compute their point of intersection with a ray.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Segment where

import           Control.Applicative     (liftA2)
import           Control.Lens            ((^.))

import           Diagrams.Core

import           Diagrams.Angle
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Segment
import           Diagrams.Solve
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector
import           Diagrams.Util

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

{- All instances of Traced should maintain the invariant that the list of
   traces is sorted in increasing order.
-}

instance RealFloat n => Traced (Segment Closed V2 n) where
  getTrace = getTrace . mkFixedSeg . (`at` origin)

instance RealFloat n => Traced (FixedSegment V2 n) where

{- Given lines defined by p0 + t0 * v0 and p1 + t1 * v1, their point of
   intersection in 2D is given by

     t_i = (v_(1-i)^ . (p1 - p0)) / (v1^ . v0)

   where v^ denotes the perpendicular to v, i.e. v rotated by
   -tau/4.

   This can be derived by starting with the parametric equation

     p0 + v0 t0 = p1 + v1 t1

   and rearranging to get the matrix equation

     [v0 -v1] [ t0 ]  =  (p1 - p0)
              [ t1 ]

   Working out the product of the inverse of [v0 -v1] with (p1 - p0)
   results in the above formulas for t_i.
-}

  getTrace (FLinear p0 p0') = mkTrace $ \p1 v1 ->
    let
      v0     = p0' .-. p0
      det    = perp v1 `dot` v0
      p      = p1 .-. p0
      t0     = (perp v1 `dot` p) / det
      t1     = (perp v0 `dot` p) / det
    in
      mkSortedList $
        if det == 0 || t0 < 0 || t0 > 1
          then []
          else [t1]

{- To do intersection of a line with a cubic Bezier, we first rotate
   and scale everything so that the line has parameters (origin, unitX);
   then we find the intersection(s) of the Bezier with the x-axis.

   XXX could we speed this up by first checking whether all the
   control point y-coordinates lie on the same side of the x-axis (if so,
   there can't possibly be any intersections)?  Need to set up some
   benchmarks.
-}

  getTrace bez@(FCubic {}) = mkTrace $ \p1 v1 ->
    let
      bez'@(FCubic x1 c1 c2 x2) =
        bez # moveOriginTo p1
            # rotate (negated (v1^._theta))
            # scale (1/norm v1)
      [y0,y1,y2,y3] = map (snd . unp2) [x1,c1,c2,x2]
      a  = -y0 + 3*y1 - 3*y2 + y3
      b  = 3*y0 - 6*y1 + 3*y2
      c  = -3*y0 + 3*y1
      d  = y0
      ts = filter (liftA2 (&&) (>= 0) (<= 1)) (cubForm a b c d)
      xs = map (fst . unp2 . atParam bez') ts
    in
      mkSortedList xs

