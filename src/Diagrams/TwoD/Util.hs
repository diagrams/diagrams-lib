{-# LANGUAGE FlexibleContexts
           , TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Util
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Utility functions for diagrams in two-dimensional Cartesian space.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Util
       ( -- * General two-dimensional utilities
         unitX, unitY, unit_X, unit_Y
       , direction

         -- * Size and extent of diagrams in R2
         -- ** Computing sizes
       , width, height, size2D
       , extentX, extentY, center2D

         -- ** Specifying sizes
       , SizeSpec2D(..)
       ) where

import Graphics.Rendering.Diagrams
import Diagrams.TwoD.Types

import Control.Arrow ((***), (&&&))

-- | Compute the width of a diagram.
width :: (Boundable a, V a ~ R2) => a -> Double
width = negate . uncurry (-) . extentX

-- | Compute the height of a diagram.
height :: (Boundable a, V a ~ R2) => a -> Double
height = negate . uncurry (-) . extentY

-- | Compute the width and height of a diagram.
size2D :: (Boundable a, V a ~ R2) => a -> (Double, Double)
size2D = width &&& height

-- | Compute the absolute x-coordinate range of a diagram in R2, in
--   the form (lo,hi).
extentX :: (Boundable a, V a ~ R2) => a -> (Double, Double)
extentX d = (-f (-1,0), f (1,0))
  where f = appBounds $ getBounds d

-- | Compute the absolute y-coordinate range of a diagram in R2, in
--   the form (lo,hi).
extentY :: (Boundable a, V a ~ R2) => a -> (Double, Double)
extentY d = (-f (0,-1), f (0,1))
  where f = appBounds $ getBounds d

-- | Compute the point at the center (in the x- and y-directions) of a
--   diagram.
center2D :: (Boundable a, V a ~ R2) => a -> P2
center2D = P . (mid *** mid) . (extentX &&& extentY)
  where mid = (/2) . uncurry (+)

-- | The unit vector in the positive X direction.
unitX :: R2
unitX = (1,0)

-- | The unit vector in the positive Y direction.
unitY :: R2
unitY = (0,1)

-- | The unit vector in the negative X direction.
unit_X :: R2
unit_X = (-1,0)

-- | The unit vector in the negative Y direction.
unit_Y :: R2
unit_Y = (0,-1)

-- | Compute the direction of a vector, measured counterclockwise from
--   the positive x-axis as a fraction of a full turn.  The zero
--   vector is arbitrarily assigned the direction 0.
direction :: R2 -> CircleFrac
direction (x,y) = toCircleFrac . Rad $ atan2 y x

------------------------------------------------------------
-- Size specifications
------------------------------------------------------------

-- | A specification of a (requested) rectangular size.
data SizeSpec2D = Width  Double       -- ^ Specify an explicit
                                      -- width. The height should be
                                      -- determined automatically (so
                                      -- as to preserve aspect ratio).
                | Height Double       -- ^ Specify an explicit
                                      -- height. The width should be
                                      -- determined automatically (so
                                      -- as to preserve aspect ratio)
                | Dims Double Double  -- ^ An explicit specification
                                      --   of both dimensions.
                | Absolute            -- ^ Absolute size: use whatever
                                      -- size an object already has;
                                      -- do not rescale.
