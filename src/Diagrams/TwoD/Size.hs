{-# LANGUAGE FlexibleContexts
           , TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Size
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Utilities for working with sizes of two-dimensional objects.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Size
       (
         -- * Size and extent of diagrams in R2
         -- ** Computing sizes
         width, height, size2D
       , extentX, extentY, center2D

         -- ** Specifying sizes
       , SizeSpec2D(..)
       ) where

import Graphics.Rendering.Diagrams
import Diagrams.TwoD.Types

import Control.Arrow ((***), (&&&))

------------------------------------------------------------
-- Computing diagram sizes
------------------------------------------------------------

-- | Compute the width of a boundable object.
width :: (Boundable a, V a ~ R2) => a -> Double
width = negate . uncurry (-) . extentX

-- | Compute the height of a boundable object.
height :: (Boundable a, V a ~ R2) => a -> Double
height = negate . uncurry (-) . extentY

-- | Compute the width and height of a boundable object.
size2D :: (Boundable a, V a ~ R2) => a -> (Double, Double)
size2D = width &&& height

-- | Compute the absolute x-coordinate range of a boundable object in
--   R2, in the form (lo,hi).
extentX :: (Boundable a, V a ~ R2) => a -> (Double, Double)
extentX d = (-f (-1,0), f (1,0))
  where f = appBounds $ getBounds d

-- | Compute the absolute y-coordinate range of a boundable object in
--   R2, in the form (lo,hi).
extentY :: (Boundable a, V a ~ R2) => a -> (Double, Double)
extentY d = (-f (0,-1), f (0,1))
  where f = appBounds $ getBounds d

-- | Compute the point at the center (in the x- and y-directions) of a
--   boundable object.
center2D :: (Boundable a, V a ~ R2) => a -> P2
center2D = P . (mid *** mid) . (extentX &&& extentY)
  where mid = (/2) . uncurry (+)

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
