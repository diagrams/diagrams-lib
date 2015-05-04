{-# LANGUAGE DeriveFunctor        #-}
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
       , direction, dir, fromDirection, fromDir
       , angleBetweenDirs
       , dirBetween
       ) where

import           Control.Lens   (Iso', iso)

import           Diagrams.Angle
import           Diagrams.Core

import           Linear.Affine
import           Linear.Vector
import           Linear.Metric
import           Linear.V2

--------------------------------------------------------------------------------
-- Direction

-- | A vector is described by a @Direction@ and a magnitude.  So we
-- can think of a @Direction@ as a vector that has forgotten its
-- magnitude.  @Direction@s can be used with 'fromDirection' and the
-- lenses provided by its instances.
newtype Direction v n = Dir (v n)
  deriving (Read, Show, Eq, Ord, Functor) -- todo: special instances

type instance V (Direction v n) = v
type instance N (Direction v n) = n

instance (V (v n) ~ v, N (v n) ~ n, Transformable (v n)) => Transformable (Direction v n) where
  transform t (Dir v) = Dir (transform t v)

instance HasTheta v => HasTheta (Direction v) where
  _theta = _Dir . _theta

instance HasPhi v => HasPhi (Direction v) where
  _phi = _Dir . _phi

-- | _Dir is provided to allow efficient implementations of functions
--   in particular vector-spaces, but should be used with care as it
--   exposes too much information.
_Dir :: Iso' (Direction v n) (v n)
_Dir = iso (\(Dir v) -> v) Dir

-- | @direction v@ is the direction in which @v@ points.  Returns an
--   unspecified value when given the zero vector as input.
direction :: v n -> Direction v n
direction = Dir

-- | Synonym for 'direction'.
dir :: v n -> Direction v n
dir = Dir

-- | @fromDirection d@ is the unit vector in the direction @d@.
fromDirection :: (Metric v, Floating n) => Direction v n -> v n
fromDirection (Dir v) = signorm v

-- | Synonym for 'fromDirection'.
fromDir :: (Metric v, Floating n) => Direction v n -> v n
fromDir (Dir v) = signorm v

-- | compute the positive angle between the two directions in their common plane
angleBetweenDirs :: (Metric v, Floating n, Ord n, R2 v)
  => Direction v n -> Direction v n -> Angle n
angleBetweenDirs d1 d2 = angleBetween (fromDirection d1) (fromDirection d2)

-- | @dirBetween p q@ returns the directions from @p@ to @q@
dirBetween :: (Additive v, Num n) => Point v n -> Point v n -> Direction v n
dirBetween p q = dir $ p .-. q

