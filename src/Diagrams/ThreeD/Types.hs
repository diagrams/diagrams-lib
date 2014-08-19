{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

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
       , r3Iso, p3Iso
        , ThreeD
       -- * other coördinate systems
       , Spherical(..), Cylindrical(..), HasPhi(..)
       ) where

import           Control.Lens           (Iso', iso)

import           Diagrams.Angle
import           Diagrams.Coordinates
import           Diagrams.Core
import           Diagrams.Points

import           Data.AffineSpace.Point
import           Data.Basis
import           Data.Cross
import           Data.Typeable
import           Data.VectorSpace

------------------------------------------------------------
-- 3D Euclidean space

type R3Basis = Either () (Either () ())

-- Basic R3 types

type ScalarThreeD d = (Ord d, Scalar d ~ d, InnerSpace d, RealFloat d)
type ThreeD v = (HasBasis v, Basis v ~ R3Basis, Coordinates v, Coordinates (PrevDim v), PrevDim (PrevDim v) ~ Scalar v, FinalCoord (PrevDim v) ~ Scalar v, FinalCoord v ~ Scalar v, Decomposition v ~ (Scalar v :& Scalar v :& Scalar v), v ~ V v, Typeable v, ScalarR3Ish (Scalar v), Transformable v, InnerSpace v, HasCross3 v, HasX v, HasY v, HasZ v, HasTheta v, Cylindrical v)

r3Iso :: (ThreeD v) => Iso' v (Scalar v, Scalar v, Scalar v)
r3Iso = iso unr3 r3

-- | Construct a 3D vector from a triple of components.
r3 :: (ThreeD v) => (Scalar v, Scalar v, Scalar v) -> v
r3 (x,y,z) = x ^& y ^& z

-- | Curried version of `r3`.
mkR3 :: (ThreeD v) => Scalar v -> Scalar v -> Scalar v -> v
mkR3 x y z = x ^& y ^& z

-- | Convert a 3D vector back into a triple of components.
unr3 :: (ThreeD v) => v -> (Scalar v, Scalar v, Scalar v)
unr3 (coords -> (coords -> x :& y) :& z) = (x,y,z)

-- | Construct a 3D point from a triple of coordinates.
p3 :: (ThreeD v) => (Scalar v, Scalar v, Scalar v) -> Point v
p3 = P . r3

-- | Convert a 3D point back into a triple of coordinates.
unp3 :: (ThreeD v) => Point v -> (Scalar v, Scalar v, Scalar v)
unp3 = unr3 . unPoint

p3Iso :: (ThreeD v) => Iso' (Point v) (Scalar v, Scalar v, Scalar v)
p3Iso = iso unp3 p3

-- | Curried version of `r3`.
mkP3 :: (ThreeD v) => Scalar v -> Scalar v -> Scalar v -> Point v
mkP3 x y z = p3 (x, y, z)

-- | Types which can be expressed in spherical 3D coordinates, as a
-- triple (r,θ,φ), where θ is rotation about the Z axis, and φ is the
-- angle from the Z axis.
class Spherical t where
    spherical :: Iso' t (Scalar (V t), Angle (Scalar (V t)), Angle (Scalar (V t)))

-- | Types which can be expressed in cylindrical 3D coordinates.
class Cylindrical t where
    cylindrical :: Iso' t (Scalar (V t), Angle (Scalar (V t)), Scalar (V t)) -- r, θ, z

instance (Cylindrical v, v ~ V v) => Cylindrical (Point v) where
    cylindrical = _pIso . cylindrical

instance (Spherical v, v ~ V v) => Spherical (Point v) where
    spherical = _pIso . spherical

