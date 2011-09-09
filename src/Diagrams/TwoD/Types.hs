{-# LANGUAGE TypeFamilies
           , TypeSynonymInstances
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Types
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Basic types for two-dimensional Euclidean space.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Types
       ( -- * 2D Euclidean space
         R2
       , P2
       , T2

         -- * Angles
       , Angle(..)
       , CircleFrac(..), Rad(..), Deg(..)
       , fullCircle, convertAngle
       ) where

import Graphics.Rendering.Diagrams

import Math.Tau

------------------------------------------------------------
-- 2D Euclidean space

-- | The two-dimensional Euclidean vector space R^2.
type R2 = (Double, Double)

type instance V R2 = R2

-- | Points in R^2.
type P2 = Point R2

-- | Transformations in R^2.
type T2 = Transformation R2

instance Transformable R2 where
  transform = apply

------------------------------------------------------------
-- Angles

-- | Newtype wrapper used to represent angles as fractions of a
--   circle.  For example, 1/3 = tau/3 radians = 120 degrees.
newtype CircleFrac = CircleFrac { getCircleFrac :: Double }
  deriving (Read, Show, Eq, Ord, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

-- | Newtype wrapper for representing angles in radians.
newtype Rad = Rad { getRad :: Double }
  deriving (Read, Show, Eq, Ord, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

-- | Newtype wrapper for representing angles in degrees.
newtype Deg = Deg { getDeg :: Double }
  deriving (Read, Show, Eq, Ord, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

-- | Type class for types that measure angles.
class Num a => Angle a where
  -- | Convert to a fraction of a circle.
  toCircleFrac   :: a -> CircleFrac

  -- | Convert from a fraction of a circle.
  fromCircleFrac :: CircleFrac -> a

instance Angle CircleFrac where
  toCircleFrac   = id
  fromCircleFrac = id

-- | tau radians = 1 full circle.
instance Angle Rad where
  toCircleFrac   = CircleFrac . (/tau) . getRad
  fromCircleFrac = Rad . (*tau) . getCircleFrac

-- | 360 degrees = 1 full circle.
instance Angle Deg where
  toCircleFrac   = CircleFrac . (/360) . getDeg
  fromCircleFrac = Deg . (*360) . getCircleFrac

-- | An angle representing a full circle.
fullCircle :: Angle a => a
fullCircle = fromCircleFrac 1

-- | Convert between two angle representations.
convertAngle :: (Angle a, Angle b) => a -> b
convertAngle = fromCircleFrac . toCircleFrac