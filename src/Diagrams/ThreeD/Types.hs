{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

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
         R3(..), r3, unr3
       , P3, p3, unp3
       , T3
       , r3Iso

         -- * Two-dimensional angles
         -- | These are defined in "Diagrams.TwoD.Types" but
         --   reëxported here for convenience.
       , Angle(..)
       , Turn(..), asTurn, CircleFrac, Rad(..), asRad, Deg(..), asDeg
       , fullTurn, convertAngle, angleRatio

         -- * Directions in 3D
       , Direction(..)
       , Spherical(..)
       , asSpherical
       ) where

import           Control.Applicative
import           Control.Lens           (Iso', iso, over, _1, _2, _3)

import           Diagrams.Coordinates
import           Diagrams.Core
import           Diagrams.TwoD.Types

import           Data.AffineSpace.Point
import           Data.Basis
import           Data.Cross
import           Data.VectorSpace

------------------------------------------------------------
-- 3D Euclidean space

-- | The three-dimensional Euclidean vector space R^3.
newtype R3 = R3 { unR3 :: (Double, Double, Double) }
  deriving (AdditiveGroup, Eq, Ord, Show, Read)

r3Iso :: Iso' R3 (Double, Double, Double)
r3Iso = iso unR3 R3

-- | Construct a 3D vector from a triple of components.
r3 :: (Double, Double, Double) -> R3
r3 = R3

-- | Convert a 3D vector back into a triple of components.
unr3 :: R3 -> (Double, Double, Double)
unr3 = unR3

type instance V R3 = R3

instance VectorSpace R3 where
  type Scalar R3 = Double
  (*^) = over r3Iso . (*^)

instance HasBasis R3 where
  type Basis R3 = Either () (Either () ()) -- = Basis (Double, Double, Double)
  basisValue = R3 . basisValue
  decompose  = decompose  . unR3
  decompose' = decompose' . unR3

instance InnerSpace R3 where
  (unR3 -> vec1) <.> (unR3 -> vec2) = vec1 <.> vec2

instance Coordinates R3 where
  type FinalCoord R3       = Double
  type PrevDim R3          = R2
  type Decomposition R3    = Double :& Double :& Double

  (coords -> x :& y) & z   = r3 (x,y,z)
  coords (unR3 -> (x,y,z)) = x :& y :& z

-- | Points in R^3.
type P3 = Point R3

-- | Construct a 3D point from a triple of coordinates.
p3 :: (Double, Double, Double) -> P3
p3 = P . R3

-- | Convert a 3D point back into a triple of coordinates.
unp3 :: P3 -> (Double, Double, Double)
unp3 = unR3 . unPoint

p3Iso :: Iso' P3 (Double, Double, Double)
p3Iso = iso unp3 p3

-- | Transformations in R^3.
type T3 = Transformation R3

instance Transformable R3 where
  transform = apply

instance HasCross3 R3 where
  cross3 u v = r3 $ cross3 (unr3 u) (unr3 v)

--------------------------------------------------------------------------------
-- Direction

-- | Direction is a type class representing directions in R3.  The interface is
-- based on that of the Angle class in 2D.

class Direction d where
    -- | Convert to polar angles
    toSpherical :: Angle a => d -> Spherical a

    -- | Convert from polar angles
    fromSpherical :: Angle a => Spherical a -> d

-- | A direction expressed as a pair of spherical coordinates.
-- `Spherical 0 0` is the direction of `unitX`.  The first coordinate
-- represents rotation about the Z axis, the second rotation towards the Z axis.
data Spherical a = Spherical a a
                   deriving (Show, Read, Eq)

instance Applicative Spherical where
    pure a = Spherical a a
    Spherical a b <*> Spherical c d = Spherical (a c) (b d)

instance Functor Spherical where
    fmap f s = pure f <*> s

instance (Angle a) => Direction (Spherical a) where
    toSpherical = fmap convertAngle
    fromSpherical = fmap convertAngle

-- | The identity function with a restricted type, for conveniently
-- restricting unwanted polymorphism.  For example, @fromDirection
-- . asSpherical . camForward@ gives a unit vector pointing in the
-- direction of the camera view.  Without @asSpherical@, the
-- intermediate type would be ambiguous.
asSpherical :: Spherical Turn -> Spherical Turn
asSpherical = id

instance HasX R3 where
    _x = r3Iso . _1

instance HasX P3 where
    _x = p3Iso . _1

instance HasY R3 where
    _y = r3Iso . _2

instance HasY P3 where
    _y = p3Iso . _2

instance HasZ R3 where
    _z = r3Iso . _3

instance HasZ P3 where
    _z = p3Iso . _3
