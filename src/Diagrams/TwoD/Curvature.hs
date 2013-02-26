{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Curvature
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Compute curvature for segments in two dimensions.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Curvature
    (
      curvature
    , squaredCurvature
    ) where

import Data.AffineSpace
import Data.VectorSpace

import Control.Arrow (first, second)
import Control.Monad (join)

import Diagrams.Core

import Diagrams.Segment
import Diagrams.TwoD.Types
import Diagrams.TwoD.Vector

-- | Curvature measures how curved the segment is at a point.  One intuition
-- for the concept is how much you would turn the wheel when driving a car
-- along the curve.  When the wheel is held straight there is zero curvature.
-- When turning a corner to the left we will have positive curvature.  When
-- turning to the right we will have negative curvature.
--
-- Another way to measure this idea is to find the largest circle that we can
-- push up against the curve and have it touch (locally) at exactly the point
-- and not cross the curve.  This is a tangent circle.  The radius of that
-- circle is the \"Radius of Curvature\" and it is the reciprocal of curvature.
-- Note that if the circle is on the \"left\" of the curve, we have a positive
-- radius, and if it is to the right we have a negative radius.  Straight
-- segments have an infinite radius which leads us to our representation.  We
-- result in a pair of numerator and denominator so we can include infinity and
-- zero for both the radius and the curvature.
--
--
-- Lets consider the following curve:
--
-- <<diagramA.svg#diagram=diagramA&height=200&width=400>>
--
-- The curve starts with positive curvature,
--
-- <<diagramPos.svg#diagram=diagramPos&height=200&width=400>>
--
-- approaches zero curvature
--
-- <<diagramZero.svg#diagram=diagramZero&height=200&width=400>>
--
-- then has negative curvature
--
-- <<diagramNeg.svg#diagram=diagramNeg&height=200&width=400>>
--
-- > import Diagrams.TwoD.Curvature
-- > 
-- > segmentA = Cubic (12 & 0) (8 & 10) (20 & 8)
-- > 
-- > curveA = lw 0.1 . stroke . fromSegments $ [segmentA]
-- > 
-- > diagramA = pad 1.1 . centerXY $ curveA
-- > 
-- > diagramPos = diagramWithRadius 0.2
-- > 
-- > diagramZero = diagramWithRadius 0.5
-- > 
-- > diagramNeg = diagramWithRadius 0.8
-- > 
-- > diagramWithRadius t = pad 1.1 . centerXY 
-- >          $ curveA 
-- >         <> showCurvature segmentA t
-- >          # withEnvelope (curveA :: D R2)
-- >          # lw 0.05 # lc red
-- > 
-- > showCurvature bez@(Cubic b c d) t
-- >   | p == 0 || v == 0 = mempty
-- >   | otherwise        = (circle (abs r) # translate vpr
-- >                     <> stroke (origin ~~ (origin .+^ vpr)))
-- >                      # moveTo (origin .+^ atParam bez t)
-- >   where
-- >     vpr = r2 (normalized vp ^* r)
-- >     v@(x,y) = unr2 $ firstDerivative b c d t
-- >     vp = (-y) & x
-- >     (p, q) = curvature bez t
-- >     r = q / p
-- >     firstDerivative b c d t = let tt = t*t in (3*(3*tt-4*t+1))*^b + (3*(2-3*t)*t)*^c + (3*tt)*^d
--
curvature :: Segment R2       -- ^ Segment to measure on.
          -> Double           -- ^ Parameter to measure at.
          -> (Double, Double) -- ^ Result is a numerator denominator pair (p,q) where
                              -- the curvature is p\/q and radius of curvature is q\/p
curvature s = second sqrt . curvaturePair (fmap unr2 s) -- TODO: Use the generalized unr2

-- | With @squaredCurvature@ we can compute values in spaces that do not support
-- 'sqrt' and it is just as useful for relative ordering of curvatures or looking
-- for zeros.
squaredCurvature :: Segment R2 -> Double -> (Double, Double)
squaredCurvature s = first (join (*)) . curvaturePair (fmap unr2 s) -- TODO: Use the generalized unr2

-- Internal function that is not quite curvature or squaredCurvature but lets
-- us get there by either taking the square root of the numerator or squaring
-- the denominator respectively.
curvaturePair :: (Num t, Num (Scalar t), VectorSpace t)
    => Segment (t, t) -> Scalar t -> (t, t)
curvaturePair (Linear _)    t = (0,1) -- Linear segments always have zero curvature (infinite radius).
curvaturePair (Cubic b c d) t = ((x'*y'' - y'*x''), (x'*x' + y'*y')^3)
  where
    (x' ,y' ) = firstDerivative  b c d t -- TODO: Use the generalized unr2
    (x'',y'') = secondDerivative b c d t

    firstDerivative b c d t = (3*(3*tt-4*t+1))*^b + (3*(2-3*t)*t)*^c + (3*tt)*^d
      where
        tt = t * t

    secondDerivative b c d t = (6*(3*t-2))*^b + (6-18*t)*^c + (6*t)*^d

-- TODO: We should be able to generalize this to higher dimensions.  See
-- <http://en.wikipedia.org/wiki/Curvature>
--
-- TODO: I'm not sure what the best way to generalize squaredCurvature to other spaces is.

