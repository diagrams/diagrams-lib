{-# LANGUAGE FlexibleContexts
           , TypeSynonymInstances
           , MultiParamTypeClasses
           , TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Ellipse
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Two-dimensional ellipses (and, as a special case, circles).
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Ellipse
    (
      -- * Ellipse and circle diagrams
      unitCircle
    , circle
    , ellipse
    , ellipseXY

      -- * Mathematical ellipses
      -- ** Representation
    , Ellipse(..)

      -- ** Extracting attributes
    , ellipseCenter
    , ellipseAngle
    , ellipseAxes
    , ellipseScale

    , ellipseCoeffs

    ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform
import Diagrams.Util

import Data.Monoid (Any(..), mempty)

import Data.VectorSpace (magnitudeSq, magnitude, (^-^))

-- | An ellipse is represented by an affine transformation acting on
--   the unit circle.
data Ellipse = Ellipse T2

type instance V Ellipse = R2

instance Transformable Ellipse where
  transform t (Ellipse e) = Ellipse (t <> e)

-- | A circle of radius 1, with center at the origin.
unitCircle :: (Backend b R2, Renderable Ellipse b) => Diagram b R2
unitCircle = mkAD (Prim $ Ellipse mempty)
                  (Bounds circleBounds)
                  mempty
                  (Query circleQuery)
  where circleBounds (x,y) = 1 / sqrt(x*x + y*y)
        circleQuery (P (x,y)) = Any $ x*x + y*y <= 1

-- | A circle of the given radius, centered at the origin.
circle :: (Backend b R2, Renderable Ellipse b) => Double -> Diagram b R2
circle d = unitCircle # scale d

-- | @ellipse e@ constructs an ellipse with eccentricity @e@ by
--   scaling the unit circle in the X direction.  The eccentricity must
--   be within the interval [0,1).
ellipse :: (Backend b R2, Renderable Ellipse b) => Double -> Diagram b R2
ellipse e
    | e >= 0 && e < 1  = scaleX (sqrt (1 - e*e)) unitCircle
    | otherwise        = error "Eccentricity of ellipse must be >= 0 and < 1."

-- | @ellipseXY x y@ creates an axis-aligned ellipse, centered at the
--   origin, with radius @x@ along the x-axis and radius @y@ along the
--   y-axis.
ellipseXY :: (Backend b R2, Renderable Ellipse b) => Double -> Double -> Diagram b R2
ellipseXY x y = unitCircle # scaleX x # scaleY y

-- | Compute the coefficients of the quadratic form
--
--     A x^2 + B x y + C y^2 + D x + E y + F = 0
--
--   for an ellipse.  Returns A through F (in that order) as a tuple.
ellipseCoeffs :: Ellipse -> (Double, Double, Double, Double, Double, Double)
ellipseCoeffs (Ellipse eT) = (      a*a + d*d      -- x^2
                             , 2 * (a*b + d*e)     -- xy
                             ,      b*b + e*e      -- y^2
                             , 2 * (a*c + d*f)     -- x
                             , 2 * (b*c + e*f)     -- y
                             ,      c*c + f*f - 1
                             )
  where eT'   = inv eT
        (a,d) = apply eT' (1,0)
        (b,e) = apply eT' (0,1)
        (c,f) = transl eT'

-- | Compute the center of an ellipse.
ellipseCenter :: Ellipse -> P2
ellipseCenter (Ellipse e) = papply e origin

-- | Compute the angle to the major axis of an ellipse, measured
--   counterclockwise from the positive x axis in radians.  The result
--   will be in the range [0, tau/2).
ellipseAngle :: Ellipse -> Rad
ellipseAngle ell
  | y < 0     = Rad $ pi + atan2 y x
  | otherwise = Rad $ atan2 y x
  where ((x,y),_) = ellipseAxes ell

-- | Compute the vectors (va, vb) from the center of the ellipse to the edge of the
--   ellipse along the major and minor axes.  These vectors can lie in any quadrant,
--   depending on how the ellipse has been transformed.
ellipseAxes :: Ellipse -> (R2, R2)
ellipseAxes (Ellipse eT) = if magnitudeSq va >= magnitudeSq vb then (va,vb) else (vb,va)
  where a     = apply eT (1,0)
        b     = apply eT (0,1)
        v     = apply eT (0,0)
        va    = a ^-^ v
        vb    = b ^-^ v

-- | Compute the scaling factors of an ellipse, i.e. (a,b) where a and
--   b are half the lengths of the major and minor axes respectively.
ellipseScale :: Ellipse -> (Double, Double)
ellipseScale ell = (magnitude a, magnitude b)
  where (a,b) = ellipseAxes ell
