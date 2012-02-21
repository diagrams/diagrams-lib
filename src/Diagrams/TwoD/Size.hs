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
import Diagrams.TwoD.Vector

import Control.Arrow ((***), (&&&))
import Control.Applicative ((<$>), liftA2)

------------------------------------------------------------
-- Computing diagram sizes
------------------------------------------------------------

-- | Compute the width of an enveloped object.
width :: (Enveloped a, V a ~ R2) => a -> Double
width = maybe 0 (negate . uncurry (-)) . extentX

-- | Compute the height of an enveloped object.
height :: (Enveloped a, V a ~ R2) => a -> Double
height = maybe 0 (negate . uncurry (-)) . extentY

-- | Compute the width and height of an enveloped object.
size2D :: (Enveloped a, V a ~ R2) => a -> (Double, Double)
size2D = width &&& height

-- | Compute the absolute  x-coordinate range of an enveloped object in
--   R2, in  the form (lo,hi).   Return @Nothing@ for objects  with an
--   empty envelope.
extentX :: (Enveloped a, V a ~ R2) => a -> Maybe (Double, Double)
extentX d = (\f -> (-f unit_X, f unitX)) <$> (appEnvelope . getEnvelope $ d)

-- | Compute the absolute y-coordinate range of an enveloped object in
--   R2, in the form (lo,hi).
extentY :: (Enveloped a, V a ~ R2) => a -> Maybe (Double, Double)
extentY d = (\f -> (-f unit_Y, f unitY)) <$> (appEnvelope . getEnvelope $ d)

-- | Compute the point at the center (in the x- and y-directions) of a
--   enveloped object.  Return the origin for objects with an empty
--   envelope.
center2D :: (Enveloped a, V a ~ R2) => a -> P2
center2D = maybe origin (p2 . (mid *** mid)) . mm . (extentX &&& extentY)
  where mm = uncurry (liftA2 (,))
        mid = (/2) . uncurry (+)

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
                                      -- of both dimensions.
                | Absolute            -- ^ Absolute size: use whatever
                                      -- size an object already has;
                                      -- do not rescale.
