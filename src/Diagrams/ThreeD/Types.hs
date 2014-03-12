{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

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
         R3(..), r3, unr3, mkR3
       , P3, p3, unp3, mkP3
       , T3
       , r3Iso, p3Iso

         -- * Directions in 3D
       , Direction, direction, fromDirection, angleBetweenDirs
       -- * other coördinate systems
       , Spherical(..), Cylindrical(..), HasPhi(..)
       ) where

import           Control.Lens           (Iso', Lens', iso, over
                                        , _1, _2, _3, (^.))

import           Diagrams.Core
import           Diagrams.Angle
import           Diagrams.TwoD.Types    (R2)
import           Diagrams.Coordinates

import           Data.AffineSpace.Point
import           Data.Basis
import           Data.Cross
import           Data.VectorSpace

------------------------------------------------------------
-- 3D Euclidean space

-- | The three-dimensional Euclidean vector space R^3.
data R3 = R3 !Double !Double !Double
  deriving (Eq, Ord, Show, Read)

r3Iso :: Iso' R3 (Double, Double, Double)
r3Iso = iso unr3 r3

-- | Construct a 3D vector from a triple of components.
r3 :: (Double, Double, Double) -> R3
r3 (x,y,z) = R3 x y z

-- | Curried version of `r3`.
mkR3 :: Double -> Double -> Double -> R3
mkR3 = R3

-- | Convert a 3D vector back into a triple of components.
unr3 :: R3 -> (Double, Double, Double)
unr3 (R3 x y z) = (x,y,z)

instance AdditiveGroup R3 where
    zeroV = R3 0 0 0
    R3 x1 y1 z1 ^+^ R3 x2 y2 z2 = R3 (x1 + x2) (y1 + y2) (z1 + z2)
    negateV (R3 x y z) = R3 (-x) (-y) (-z)

type instance V R3 = R3

instance VectorSpace R3 where
  type Scalar R3 = Double
  (*^) = over r3Iso . (*^)

instance HasBasis R3 where
  type Basis R3 = Either () (Either () ()) -- = Basis (Double, Double, Double)
  basisValue = r3 . basisValue
  decompose  = decompose  . unr3
  decompose' = decompose' . unr3

instance InnerSpace R3 where
    (R3 x1 y1 z1) <.> (R3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

instance Coordinates R3 where
  type FinalCoord R3       = Double
  type PrevDim R3          = R2
  type Decomposition R3    = Double :& Double :& Double

  (coords -> x :& y) ^& z   = R3 x y z
  coords (R3 x y z) = x :& y :& z

-- | Points in R^3.
type P3 = Point R3

-- | Construct a 3D point from a triple of coordinates.
p3 :: (Double, Double, Double) -> P3
p3 = P . r3

-- | Convert a 3D point back into a triple of coordinates.
unp3 :: P3 -> (Double, Double, Double)
unp3 = unr3 . unPoint

p3Iso :: Iso' P3 (Double, Double, Double)
p3Iso = iso unp3 p3

-- | Curried version of `r3`.
mkP3 :: Double -> Double -> Double -> P3
mkP3 x y z = p3 (x, y, z)

-- | Transformations in R^3.
type T3 = Transformation R3

instance Transformable R3 where
  transform = apply

instance HasCross3 R3 where
  cross3 u v = r3 $ cross3 (unr3 u) (unr3 v)

--------------------------------------------------------------------------------
-- Direction

-- | A @Direction@ represents directions in R3.  The constructor is
-- not exported; @Direction@s can be used with 'fromDirection' and the
-- lenses provided by its instances.
data Direction = Direction R3

-- | Not exported
_Dir :: Iso' Direction R3
_Dir = iso (\(Direction v) -> v) Direction

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

-- | Types which can be expressed in spherical 3D coordinates, as a
-- triple (r,θ,φ), where θ is rotation about the Z axis, and φ is the
-- angle from the Z axis.
class Spherical t where
    spherical :: Iso' t (Double, Angle, Angle)

-- | Types which can be expressed in cylindrical 3D coordinates.
class Cylindrical t where
    cylindrical :: Iso' t (Double, Angle, Double) -- r, θ, z

instance Cylindrical R3 where
    cylindrical = iso (\(R3 x y z) -> (sqrt (x^(2::Int)+y^(2::Int)), atanA (y/x), z))
                      (\(r,θ,z) -> R3 (r*cosA θ) (r*sinA θ) z)

instance Spherical R3 where
    spherical = iso
      (\v@(R3 x y z) -> (magnitude v, atanA (y/x), atanA (v^._r/z)))
      (\(r,θ,φ) -> R3 (r*cosA θ*sinA φ) (r*sinA θ*sinA φ) (r*cosA φ))

-- We'd like to write: instance Spherical t => HasR t
-- But GHC can't work out that the instance won't overlap.  Just write them explicitly:

instance HasR R3 where
    _r = spherical . _1

instance HasR P3 where
    _r = spherical . _1

instance HasTheta R3 where
    _theta = cylindrical . _2

instance HasTheta P3 where
    _theta = cylindrical . _2

-- | The class of types with at least two angle coordinates, the
-- second called _phi.
class HasPhi t where
    _phi :: Lens' t Angle

instance HasPhi R3 where
    _phi = spherical . _3

instance HasPhi P3 where
    _phi = spherical . _3

instance Cylindrical P3 where
    cylindrical = _relative origin . cylindrical

instance Spherical P3 where
    spherical = _relative origin . spherical

instance HasTheta Direction where
    _theta = _Dir . _theta

instance HasPhi Direction where
    _phi = _Dir . _phi

-- | @direction v@ is the direction in which @v@ points.  Returns an
--   unspecified value when given the zero vector as input.
direction :: R3 -> Direction
direction = Direction

-- | @fromDirection d@ is the unit vector in the direction @d@.
fromDirection :: Direction -> R3
fromDirection (Direction v) = normalized v

-- | compute the positive angle between the two directions in their common plane
angleBetweenDirs  :: Direction -> Direction -> Angle
angleBetweenDirs d1 d2 = angleBetween (fromDirection d1) (fromDirection d2)
