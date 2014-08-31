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
         V2 (..), P2, R1 (..), R2 (..)
       , r2, unr2, mkR2, r2Iso
       , p2, mkP2, unp2, p2Iso
       , r2polarIso
       , project
       , HasR (..)
       ) where

import Control.Lens (Lens', Iso', iso, _1, _2)

import Diagrams.Angle
import Diagrams.Points

import Diagrams.Core.Transform
import Diagrams.Core.V
import Linear.Metric
import Linear.V2
import Linear.Vector

type P2 = Point V2

type instance V (V2 n) = V2
type instance N (V2 n) = n

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

-- | @project u v@ computes the projection of @v@ onto @u@.
project :: (Metric v, Fractional n) => v n -> v n -> v n
project u v = ((v `dot` u) / quadrance u) *^ u
-- find somewhere better for this

instance Transformable (V2 n) where
  transform = apply

r2polarIso :: RealFloat n => Iso' (V2 n) (n, Angle n)
r2polarIso = iso (\v@(V2 x y) -> (norm v, atan2A y x))
              (\(r,θ)      -> V2 (r * cosA θ) (r * sinA θ))
{-# INLINE r2polarIso #-}

-- | A space which has magnitude '_r' that can be calculated numerically.
class HasR t where
  _r :: RealFloat n => Lens' (t n) n

instance HasR v => HasR (Point v) where
  _r = lensP . _r
  {-# INLINE _r #-}

instance HasR V2 where
  _r = r2polarIso . _1
  {-# INLINE _r #-}

instance HasTheta V2 where
  _theta = r2polarIso . _2
  {-# INLINE _theta #-}

