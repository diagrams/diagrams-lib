{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Types
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Basic types for three-dimensional Euclidean space.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Types
       ( -- * 3D Euclidean space
         r3, unr3, mkR3
       , p3, unp3, mkP3
       , r3Iso, p3Iso, project
       , V3 (..), P3
       , R1 (..), R2 (..), R3 (..)

       -- * other coördinate systems
       , Spherical(..), Cylindrical(..), HasPhi(..)
       ) where

import           Control.Lens           (Iso', iso, _2)

import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.Points

import Linear.V3 as V
import Linear.Metric
import Linear.Vector

------------------------------------------------------------
-- 3D Euclidean space

-- Basic R3 types

-- type R3 = V3
type P3 = Point V3

r3Iso :: Iso' (V3 n) (n, n, n)
r3Iso = iso unr3 r3

-- | Construct a 3D vector from a triple of components.
r3 :: (n, n, n) -> V3 n
r3 (x,y,z) = V3 x y z

-- | Curried version of `r3`.
mkR3 :: n -> n -> n -> V3 n
mkR3 = V3

-- | Convert a 3D vector back into a triple of components.
unr3 :: V3 n -> (n, n, n)
unr3 (V3 x y z) = (x,y,z)

-- | Construct a 3D point from a triple of coordinates.
p3 :: (n, n, n) -> P3 n
p3 = P . r3

-- | Convert a 3D point back into a triple of coordinates.
unp3 :: P3 n -> (n, n, n)
unp3 (P (V3 x y z)) = (x,y,z)

p3Iso :: Iso' (P3 n) (n, n, n)
p3Iso = iso unp3 p3

-- | Curried version of `r3`.
mkP3 :: n -> n -> n -> P3 n
mkP3 x y z = p3 (x, y, z)


-- | @project u v@ computes the projection of @v@ onto @u@.
project :: (Metric v, Fractional n) => v n -> v n -> v n
project u v = ((v `dot` u) / quadrance u) *^ u

-- | Types which can be expressed in spherical 3D coordinates, as a
-- triple (r,θ,φ), where θ is rotation about the Z axis, and φ is the
-- angle from the Z axis.
class Spherical v where
  spherical :: RealFloat n => Iso' (v n) (n, Angle n, Angle n)

-- | Types which can be expressed in cylindrical 3D coordinates.
class Cylindrical v where
  cylindrical :: Floating n => Iso' (v n) (n, Angle n, n) -- r, θ, z

instance Cylindrical v => Cylindrical (Point v) where
  cylindrical = _pIso . cylindrical

instance Spherical v => Spherical (Point v) where
  spherical = _pIso . spherical

type instance V (V3 n) = V3
type instance N (V3 n) = n

instance Transformable (V3 n) where
	transform = apply

instance Cylindrical V3 where
  cylindrical = iso
    (\(V3 x y z) -> (sqrt (sq x + sq y), atanA (y/x), z))
    (\(r,θ,z)    -> V3 (r*cosA θ) (r*sinA θ) z)
    where sq x = x * x

instance Spherical V3 where
  spherical = iso
    (\v@(V3 x y z) -> (norm v, atan2A y x, acosA (z / norm v)))
    (\(r,θ,φ)   -> V3 (r * cosA θ * sinA φ) (r * sinA θ * sinA φ) (r * cosA φ))

    -- spherical = iso
    --   (\v@(R3 x y z) -> (magnitude v, atanA (y/x), atanA (v^._r/z)))
    --   (\(r,θ,φ) -> R3 (r*cosA θ*sinA φ) (r*sinA θ*sinA φ) (r*cosA φ))

-- We'd like to write: instance Spherical t => HasR t
-- But GHC can't work out that the instance won't overlap.  Just write them explicitly:

-- instance HasR V3 where
--   _r = spherical . _1

instance HasTheta V3 where
  _theta = cylindrical . _2

-- instance HasPhi V3 where
--   _phi = spherical . _3
