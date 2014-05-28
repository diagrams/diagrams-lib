{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances, ConstraintKinds  #-}
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
    ) where

import           Diagrams.Core

import           Diagrams.Angle
import           Diagrams.Located        (at)
import           Diagrams.TrailLike
import           Diagrams.Trail (glueTrail)
import           Diagrams.TwoD.Arc
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector    (xDir)
import           Diagrams.Util
import           Data.VectorSpace

-- | A circle of radius 1, with center at the origin.
unitCircle :: (TrailLike t, ExtraLikeR2 (V t)) => t
unitCircle = trailLike $ glueTrail (arcT xDir fullTurn) `at` (p2 (1,0))

-- | A circle of the given radius, centered at the origin.  As a path,
--   it begins at (r,0).
circle :: (TrailLike t, ExtraLikeR2 (V t), Transformable t) => Scalar (V t) -> t
circle d = unitCircle # scale d

-- | @ellipse e@ constructs an ellipse with eccentricity @e@ by
--   scaling the unit circle in the X direction.  The eccentricity must
--   be within the interval [0,1).
ellipse :: (TrailLike t, ExtraLikeR2 (V t), Transformable t) => Scalar (V t) -> t
ellipse e
    | e >= 0 && e < 1  = scaleX (sqrt (1 - e*e)) unitCircle
    | otherwise        = error "Eccentricity of ellipse must be >= 0 and < 1."

-- | @ellipseXY x y@ creates an axis-aligned ellipse, centered at the
--   origin, with radius @x@ along the x-axis and radius @y@ along the
--   y-axis.
ellipseXY :: (TrailLike t, ExtraLikeR2 (V t), Transformable t) => Scalar (V t) -> Scalar (V t) -> t
ellipseXY x y = unitCircle # scaleX x # scaleY y
