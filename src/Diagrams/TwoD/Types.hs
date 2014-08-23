{-# LANGUAGE TypeFamilies #-}

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
         V2 (..), R1 (..), R2 (..), P2, r2, unr2, mkR2, r2Iso
       , p2, mkP2, unp2, p2Iso, project, perp
       , Polar(..)
       ) where

import           Control.Lens           (Iso', iso, _2)

import           Diagrams.Angle
-- import           Diagrams.Coordinates
import           Diagrams.Points

import Linear.Vector
import Linear.Metric
import Linear.V2
import Diagrams.Core.V
import Diagrams.Core.Transform


type P2 = Point V2

type instance V (V2 n) = V2
type instance N (V2 n) = n

-- type ScalarR2Ish d = (RealFloat d, VectorSpace d, HasBasis d, Basis d ~ (), Transformable d, Scalar d ~ d, V d ~ d, Typeable d)
-- type R2Ish v = (HasBasis v, Basis v ~ R2Basis, V v ~ v, Transformable v, InnerSpace v, Coordinates v, Decomposition v ~ (FinalCoord v :& FinalCoord v), PrevDim v ~ FinalCoord v, FinalCoord v ~ Scalar v, HasX v, HasY v, ScalarR2Ish (Scalar v), HasTheta v, Typeable v)

-- type R2D v = (R2Ish v, Data v, Data (Scalar v))

-- | Construct a 2D vector from a pair of components.  See also '&'.
r2 :: (n, n) -> V2 n
r2 = uncurry V2

-- | Convert a 2D vector back into a pair of components.  See also 'coords'.
unr2 :: V2 n -> (n, n)
unr2 (V2 x y) = (x, y)

-- | Curried form of `r2`.
mkR2 :: n -> n -> V2 n
mkR2 = V2

r2Iso :: Iso' (V2 n) (n, n)
r2Iso = iso unr2 r2

-- | Construct a 2D point from a pair of coordinates.  See also '^&'.
p2 :: (n, n) -> P2 n
p2 = P . uncurry V2

-- | Convert a 2D point back into a pair of coordinates.  See also 'coords'.
unp2 :: P2 n -> (n,n)
unp2 (P (V2 x y)) = (x,y)

-- | Curried form of `p2`.
mkP2 :: n -> n -> P2 n
mkP2 x = P . V2 x

p2Iso :: Iso' (Point V2 n) (n, n)
p2Iso = iso unp2 p2

-- | Types which can be expressed in polar 2D coordinates, as a magnitude and an angle.
class Polar t where
  polar :: RealFloat n => Iso' (t n) (n, Angle n)

instance Polar v => Polar (Point v) where
  polar = _pIso . polar

-- | @project u v@ computes the projection of @v@ onto @u@.
project :: (Metric v, Fractional n) => v n -> v n -> v n
project u v = ((v `dot` u) / quadrance u) *^ u
-- find somewhere better for this

-- TODO: coordinate instance for V2

instance Transformable (V2 n) where
  transform = apply

instance Polar V2 where
  polar = iso (\v@(V2 x y) -> (norm v, atan2A y x))
              (\(r,θ)      -> V2 (r * cosA θ) (r * sinA θ))

instance HasTheta V2 where
  _theta = polar . _2

