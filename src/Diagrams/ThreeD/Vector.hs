{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , ViewPatterns
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Vector
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Three-dimensional vectors.
--
-----------------------------------------------------------------------------
module Diagrams.ThreeD.Vector
       ( -- * Special 2D vectors
         unitX, unitY, unitZ, unit_X, unit_Y, unit_Z,

         -- * Converting between vectors and angles
         angleBetween, angleBetweenDirs
       ) where

import Data.VectorSpace

import Diagrams.Angle
import Diagrams.Coordinates
import Diagrams.ThreeD.Types


-- | The unit vector in the positive X direction.
unitX :: R3
unitX = 1 ^& 0 ^& 0

-- | The unit vector in the positive Y direction.
unitY :: R3
unitY = 0 ^& 1 ^& 0

-- | The unit vector in the positive Z direction.
unitZ :: R3
unitZ = 0 ^& 0 ^& 1

-- | The unit vector in the negative X direction.
unit_X :: R3
unit_X = (-1) ^& 0 ^& 0

-- | The unit vector in the negative Y direction.
unit_Y :: R3
unit_Y = 0 ^& (-1) ^& 0

-- | The unit vector in the negative Z direction.
unit_Z :: R3
unit_Z = 0 ^& 0 ^& (-1)

-- | compute the positive angle between the two vectors in their common plane
angleBetween  :: (InnerSpace v, Scalar v ~ Double) => v -> v -> Angle
angleBetween v1 v2 = acos (normalized v1 <.> normalized v2) @@ rad

-- | compute the positive angle between the two directions in their common plane
angleBetweenDirs  :: Direction -> Direction -> Angle
angleBetweenDirs d1 d2 = angleBetween (fromDirection d1) (fromDirection d2)
