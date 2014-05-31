{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns, DeriveDataTypeable               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Types.Double
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Basic types for three-dimensional Euclidean space using Double.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Types.Double
       ( -- * 3D Euclidean space
         R3(..), P3, T3
       ) where

import           Control.Lens           (iso, over, _1, _2, _3, (^.))

import           Diagrams.Core
import           Diagrams.Angle
import           Diagrams.TwoD.Types.Double(R2)
import           Diagrams.ThreeD.Types
import           Diagrams.Coordinates

import           Data.Basis
import           Data.Cross
import           Data.Typeable
import           Data.VectorSpace

------------------------------------------------------------
-- 3D Euclidean space

-- | The three-dimensional Euclidean vector space R^3.
data R3 = R3 !Double !Double !Double
  deriving (Eq, Ord, Show, Read, Typeable)

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

-- | Transformations in R^3.
type T3 = Transformation R3

instance Transformable R3 where
  transform = apply

instance HasCross3 R3 where
  cross3 u v = r3 $ cross3 (unr3 u) (unr3 v)

instance HasX R3 where
    _x = r3Iso . _1

instance HasY R3 where
    _y = r3Iso . _2

instance HasZ R3 where
    _z = r3Iso . _3

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

instance HasTheta R3 where
    _theta = cylindrical . _2

instance HasPhi R3 where
    _phi = spherical . _3
