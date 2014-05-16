{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Angle
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
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
       ) where

import Control.Lens
import Data.AffineSpace
import Data.VectorSpace

import Diagrams.Angle

--------------------------------------------------------------------------------
-- Direction

-- | A vector is described by a @Direction@ and a magnitude.  So we
-- can think of a @Direction@ as a vector that has forgotten its
-- magnitude.  @Direction@s can be used with 'fromDirection' and the
-- lenses provided by its instances.
data Direction v = Direction v

-- | _Dir is provided to allow efficient implementations of functions
-- in particular vector-spaces, but should be used with care as it
-- exposes too much information.
_Dir :: Iso' (Direction v) v
_Dir = iso (\(Direction v) -> v) Direction

-- | @direction v@ is the direction in which @v@ points.  Returns an
--   unspecified value when given the zero vector as input.
direction :: v -> Direction v
direction = Direction

-- | @fromDirection d@ is the unit vector in the direction @d@.
fromDirection :: (InnerSpace v, Floating (Scalar v)) => Direction v -> v
fromDirection (Direction v) = normalized v

-- | compute the positive angle between the two directions in their common plane
angleBetweenDirs  :: (InnerSpace v, Scalar v ~ Double) =>
                     Direction v -> Direction v -> Angle
angleBetweenDirs d1 d2 = angleBetween (fromDirection d1) (fromDirection d2)
