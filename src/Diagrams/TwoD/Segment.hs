{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
  #-}

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

import Data.AffineSpace
import Data.Monoid.PosInf
import Data.VectorSpace

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Trace

import Diagrams.Segment
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Types

instance Traced (Segment R2) where
  getTrace = getTrace . mkFixedSeg origin

{-

Given lines defined by p0 + t0 * v0 and p1 + t1 * v1, their point of
intersection in 2D is given by

  t_i = (v_(1-i)^ . (p1 - p0)) / (v1^ . v0)

where v^ denotes the perpendicular to v, i.e. v rotated by
-tau/4.

-}

instance Traced (FixedSegment R2) where
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

  getTrace (FCubic x1 c1 c2 x2) = undefined   -- XXX write me
