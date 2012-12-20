{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Applicative (liftA2)

import Data.AffineSpace
import Data.Monoid.PosInf hiding (minimum)
import Data.VectorSpace
import Data.Basis    (HasBasis(..), Basis(..))
import Data.MemoTrie (HasTrie(..))

import Diagrams.Core
import Diagrams.Core.Trace

import Diagrams.Segment
import Diagrams.Solve
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Types
import Diagrams.TwoD.Vector
import Diagrams.Util

instance ( Ord a
         , RealFloat a
         , AdditiveGroup a
         , InnerSpace a
         , HasBasis a
         , HasTrie (Basis a)
         , a ~ Scalar a
         ) => Traced (Segment (V2 a)) where
  getTrace = getTrace . mkFixedSeg origin

instance forall a. ( Ord a
         , RealFloat a
         , AdditiveGroup a
         , InnerSpace a
         , HasBasis a
         , HasTrie (Basis a)
         , a ~ Scalar a
         ) => Traced (FixedSegment (V2 a)) where

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
      det    = perp v1 <.> v0
      perp v = rotateBy (-1/4) v
      p      = p1 .-. p0
      t0     = (perp v1 <.> p) / det
      t1     = (perp v0 <.> p) / det
    in
      if det == 0 || t0 < 0 || t0 > 1
        then PosInfty
        else Finite t1

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
            # rotateBy (negate (direction v1))
            # scale (1/magnitude v1)
      [y0,y1,y2,y3] = map (snd . unp2) [x1,c1,c2,x2]
      a  = -y0 + 3*y1 - 3*y2 + y3
      b  = 3*y0 - 6*y1 + 3*y2
      c  = -3*y0 + 3*y1
      d  = y0
      ts = filter (liftA2 (&&) (>= 0) (<= 1)) (cubForm a b c d)
      xs = map (fst . unp2 . fAtParam bez') ts
    in
      case xs of
        [] -> PosInfty
        _  -> Finite (minimum xs)

