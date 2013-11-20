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
         direction, fromDirection, angleBetween, angleBetweenDirs
       ) where

import Control.Lens (from, review, (^.))
import Data.VectorSpace
import Data.Cross

import Diagrams.ThreeD.Types
import Diagrams.Coordinates


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


-- | @direction v@ is the direction in which @v@ points.  Returns an
--   unspecified value when given the zero vector as input.
direction :: Direction d => R3 -> d
direction v
  | r == 0 = fromSpherical $ Spherical zero zero
  | otherwise = fromSpherical $ Spherical θ φ where
  r = magnitude v
  (x,y,z) = unr3 v
  φ = (asin $ z / r) ^. from rad
  θ = (atan2 y $ x) ^. from rad
  zero = 0^.from rad

-- | @fromDirection d@ is the unit vector in the direction @d@.
fromDirection :: Direction d => d -> R3
fromDirection (toSpherical -> (Spherical θ' φ')) = r3 (x,y,z) where
  θ = θ'^.rad
  φ = φ'^.rad
  x = cos θ * cos φ
  y = sin θ * cos φ
  z = sin φ

-- | compute the positive angle between the two vectors in their common plane
angleBetween  :: R3 -> R3 -> Angle
angleBetween v1 v2 = review rad $
                     atan2 (magnitude $ cross3 v1 v2) (v1 <.> v2)

-- | compute the positive angle between the two vectors in their common plane
angleBetweenDirs  :: Direction d => d -> d -> Angle
angleBetweenDirs d1 d2 = angleBetween (fromDirection d1) (fromDirection d2)
