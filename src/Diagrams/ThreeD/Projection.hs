{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Projection
-- Copyright   :  (c) 2014 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- 3D projections are a way of viewing a three-dimensional objects on a
-- two-dimensional plane.
--
-- This module can be used with the functions in "Linear.Projection".
--
-- Disclaimer: This module should be considered experimental and is
-- likely to change.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Projection
  ( -- * Orthographic projections

    -- $orthographic
    -- ** Parallel projections
    facingXY
  , facingXZ
  , facingYZ

    -- ** axonometric
    -- $axonometric

    -- *** Isometric projections
    -- $isometric
  , isometricApply
  , isometric

  , lookingAt

    -- ** Affine maps
  , m44AffineApply
  , m44AffineMap
  , m33AffineApply
  , m33AffineMap

    -- * Perspective projections
    -- $perspective
    -- ** Perspective deformations
  , m44Deformation
  , module Linear.Projection
  ) where

import           Control.Lens           hiding (transform)
import           Data.Functor.Rep

import           Diagrams.Core
import           Diagrams.Deform
import           Diagrams.Direction
import           Diagrams.LinearMap
import           Diagrams.ThreeD.Types  (P3)
import           Diagrams.ThreeD.Vector

import           Linear                 as L
import           Linear.Affine
import           Linear.Projection

------------------------------------------------------------------------
-- Orthographic projections
------------------------------------------------------------------------

-- $orthographic
-- Orthographic projections are a form of parallel projections where are
-- projection lines are orthogonal to the projection plane.

-- Parallel projections

-- | Look at the xy-plane with y as the up direction.
facingXY :: (Epsilon n, Floating n) => AffineMap V3 V2 n
facingXY = lookingAt unitZ origin yDir

-- | Look at the xz-plane with z as the up direction.
facingXZ :: (Epsilon n, Floating n) => AffineMap V3 V2 n
facingXZ = lookingAt unitY origin zDir

-- | Look at the yz-plane with z as the up direction.
facingYZ :: (Epsilon n, Floating n) => AffineMap V3 V2 n
facingYZ = lookingAt unitX origin zDir

-- $axonometric
-- Axonometric projections are a type of orthographic projection where
-- the object is rotated along one or more of its axes relative to the
-- plane of projection.

-- $isometric
-- Isometric projections are when the scale along each axis of the
-- projection is the same and the angle between any axis is 120
-- degrees.

-- | Apply an isometric projection given the up direction
isometricApply :: (InSpace V3 n a, InSpace V2 n b, AffineMappable a b, Floating n, Epsilon n)
               => Direction V3 n -> a -> b
isometricApply up = amap (isometric up)

-- | Make an isometric affine map with the given up direction.
isometric :: (Floating n, Epsilon n) => Direction V3 n -> AffineMap V3 V2 n
isometric up = m44AffineMap m
  where
    m = lookAt (V3 1 1 1) zero (fromDirection up)

lookingAt :: (Epsilon n, Floating n)
          => P3 n -- ^ Eye
          -> P3 n -- ^ Center
          -> Direction V3 n -- ^ Up
          -> AffineMap V3 V2 n
lookingAt (P cam) (P center) d = m44AffineMap m
  where
    m = lookAt cam center (d^._Dir)

-- | Apply the affine part of a homogeneous matrix.
m44AffineApply :: (InSpace V3 n a, InSpace V2 n b, AffineMappable a b)
               => M44 n -> a -> b
m44AffineApply = amap . m44AffineMap

-- | Create an 'AffineMap' from a 4x4 homogeneous matrix, ignoring any
--   perspective transforms.
m44AffineMap :: Num n => M44 n -> AffineMap V3 V2 n
m44AffineMap m = AffineMap (LinearMap f) (f v)
  where
    f  = view _xy . (m' !*)
    m' = m ^. linearTransform
    v  = m ^. L.translation

-- | Apply a transformation matrix and translation.
m33AffineApply :: (InSpace V3 n a, InSpace V2 n b, AffineMappable a b)
               => M33 n -> V2 n -> a -> b
m33AffineApply m = amap . m33AffineMap m

-- | Create an 'AffineMap' from a 3x3 transformation matrix and a
--   translation vector.
m33AffineMap :: Num n => M33 n -> V2 n -> AffineMap V3 V2 n
m33AffineMap m = AffineMap (LinearMap f)
  where
    f = view _xy . (m !*)

-- | Extract the linear transform part of a homogeneous matrix.
linearTransform :: (Representable u, R3 v, R3 u) => Lens' (u (v n)) (M33 n)
linearTransform = column _xyz . _xyz

------------------------------------------------------------------------
-- Perspective transforms
------------------------------------------------------------------------

-- For the time being projective transforms use the deformable class.
-- Eventually we would like to replace this with a more specialised
-- method.

-- $perspective
-- Perspective projections are when closer objects appear bigger.

-- | Make a deformation from a 4x4 homogeneous matrix.
m44Deformation :: Fractional n => M44 n -> Deformation V3 V2 n
m44Deformation m =
  Deformation (P . view _xy . normalizePoint . (m !*) . point . view _Point)

