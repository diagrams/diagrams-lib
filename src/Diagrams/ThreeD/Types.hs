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
       , r3SphericalIso, r3CylindricalIso
       , V3 (..), P3,      T3
       , R1 (..), R2 (..), R3 (..)

       ) where

import           Control.Lens        (Iso', iso, _1, _2, _3)

import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.Points
import           Diagrams.TwoD.Types

import           Linear.Metric
import           Linear.V3           as V

------------------------------------------------------------
-- 3D Euclidean space

-- Basic R3 types

type P3 = Point V3
type T3 = Transformation V3

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

type instance V (V3 n) = V3
type instance N (V3 n) = n

instance Transformable (V3 n) where
  transform = apply

r3SphericalIso :: RealFloat n => Iso' (V3 n) (n, Angle n, Angle n)
r3SphericalIso = iso
  (\v@(V3 x y z) -> (norm v, atan2A y x, acosA (z / norm v)))
  (\(r,θ,φ)   -> V3 (r * cosA θ * sinA φ) (r * sinA θ * sinA φ) (r * cosA φ))

r3CylindricalIso :: RealFloat n => Iso' (V3 n) (n, Angle n, n)
r3CylindricalIso = iso
  (\(V3 x y z) -> (sqrt $ x*x + y*y, atan2A y x, z))
  (\(r,θ,z)    -> V3 (r*cosA θ) (r*sinA θ) z)

instance HasR V3 where
  _r = r3SphericalIso . _1

instance HasTheta V3 where
  _theta = r3CylindricalIso . _2

instance HasPhi V3 where
  _phi = r3SphericalIso . _3

