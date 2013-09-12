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
         spherical, fromSpherical
       ) where

import Data.VectorSpace
import Data.Cross

import Diagrams.Coordinates
import Diagrams.ThreeD.Types

-- | The unit vector in the positive X direction.
unitX :: R3
unitX = 1 & 0 & 0

-- | The unit vector in the positive Y direction.
unitY :: R3
unitY = 0 & 1 & 0

-- | The unit vector in the positive Z direction.
unitZ :: R3
unitZ = 0 & 0 & 1

-- | The unit vector in the negative X direction.
unit_X :: R3
unit_X = (-1) & 0 & 0

-- | The unit vector in the negative Y direction.
unit_Y :: R3
unit_Y = 0 & (-1) & 0

-- | The unit vector in the negative Z direction.
unit_Z :: R3
unit_Z = 0 & 0 & (-1)

-- should this be explained in terms of τ instead?
-- | spherical p is the triple
--   (r ∈ [0,∞), θ ∈ (-π,π], φ ∈ (-π/2, π/2))
spherical :: Angle a => R3 -> (Double, a, a)
spherical v
  | r == 0 =(0, zero, zero)
  | otherwise = (r, θ, φ) where
  r = magnitude v
  (x,y,z) = unr3 v
  φ = convertAngle . Rad . asin $ z / r
  θ = convertAngle . Rad . atan2 y $ x
  zero = convertAngle . Rad $ 0

fromSpherical :: Angle a => Double -> a -> a -> R3
fromSpherical r θ' φ' = r3 (x,y,z) where
  θ = getRad $ convertAngle θ'
  φ = getRad $ convertAngle φ'
  x = r * cos θ * cos φ
  y = r * sin θ * cos φ
  z = r * sin φ

-- | compute the positive angle between the two vectors in their common plane
angleBetween  :: (Angle a, Num a, Ord a) => R3 -> R3 -> a
angleBetween v1 v2 = convertAngle . Rad $
                     atan2 (magnitude $ cross3 v1 v2) (v1 <.> v2)
