{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Direction
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Type for representing directions, polymorphic in vector space
--
-----------------------------------------------------------------------------

module Diagrams.Direction
       ( Direction
       , _Dir
       , direction, fromDirection
       , angleBetweenDirs
       ) where

import Control.Lens     (Iso', iso)

import Diagrams.Angle
import Diagrams.Core

import Linear.Metric

--------------------------------------------------------------------------------
-- Direction

-- | A vector is described by a @Direction@ and a magnitude.  So we
-- can think of a @Direction@ as a vector that has forgotten its
-- magnitude.  @Direction@s can be used with 'fromDirection' and the
-- lenses provided by its instances.
newtype Direction v n = Direction (v n)
  deriving (Read, Show, Eq, Ord) -- todo: special instances

type instance V (Direction v n) = v
type instance N (Direction v n) = n

-- instance (Transformable v, Vn (Direction v n) ~ v n) => Transformable (Direction v) where
instance (Vn (v n) ~ v n, Transformable (v n)) => Transformable (Direction v n) where
  transform t (Direction v) = Direction (transform t v)

instance HasTheta v => HasTheta (Direction v) where
  _theta = _Dir . _theta

instance HasPhi v => HasPhi (Direction v) where
  _phi = _Dir . _phi

-- | _Dir is provided to allow efficient implementations of functions
-- in particular vector-spaces, but should be used with care as it
-- exposes too much information.
_Dir :: Iso' (Direction v n) (v n)
_Dir = iso (\(Direction v) -> v) Direction

-- | @direction v@ is the direction in which @v@ points.  Returns an
--   unspecified value when given the zero vector as input.
direction :: v n -> Direction v n
direction = Direction

-- | @fromDirection d@ is the unit vector in the direction @d@.
fromDirection :: (Metric v, Floating n) => Direction v n -> v n
fromDirection (Direction v) = signorm v

-- | compute the positive angle between the two directions in their common plane
angleBetweenDirs :: (Metric v, Floating n)
  => Direction v n -> Direction v n -> Angle n
angleBetweenDirs d1 d2 = angleBetween (fromDirection d1) (fromDirection d2)

