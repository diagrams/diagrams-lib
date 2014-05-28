{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Angle
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Type for representing angles, independent of vector-space
--
-----------------------------------------------------------------------------

module Diagrams.Angle
       (
         Angle
       , rad, turn, deg
       , fullTurn, angleRatio
       , sinA, cosA, tanA, asinA, acosA, atanA, atan2A
       , (@@)
       , angleBetween
       , HasTheta(..)
       , HasPhi(..)
       ) where

import Control.Lens            (Iso', Lens', iso, review, (^.))

import Data.Monoid      hiding ((<>))
import Data.Semigroup
import Data.VectorSpace

import Diagrams.Core.V

-- | Angles can be expressed in a variety of units.  Internally,
-- they are represented in radians.
newtype Angle v = Radians v
              deriving (Read, Show, Eq, Ord, AdditiveGroup)

instance AdditiveGroup v => Semigroup (Angle v) where
    (<>) = (^+^)

instance AdditiveGroup v => Monoid (Angle v) where
    mappend = (<>)
    mempty = Radians zeroV

instance VectorSpace v => VectorSpace (Angle v) where
  type Scalar (Angle v) = Scalar v
  s *^ Radians t = Radians (s *^ t)

deriving instance InnerSpace v => InnerSpace (Angle v)

type instance V (Angle v) = V v

-- | The radian measure of an @Angle@ @a@ can be accessed as @a
-- ^. rad@.  A new @Angle@ can be defined in radians as @pi \@\@ rad@.
rad :: Iso' (Angle v) v
rad = iso (\(Radians r) -> r) Radians

-- | The measure of an @Angle@ @a@ in full circles can be accessed as
-- @a ^. turn@.  A new @Angle@ of one-half circle can be defined in as
-- @1/2 \@\@ turn@.
turn :: (VectorSpace v, Floating (Scalar v)) => Iso' (Angle v) v
turn = iso (\(Radians r) -> r ^/ (2*pi)) (Radians . (^*(2*pi)))

-- | The degree measure of an @Angle@ @a@ can be accessed as @a
-- ^. deg@.  A new @Angle@ can be defined in degrees as @180 \@\@
-- deg@.
deg :: (VectorSpace v, Floating (Scalar v)) => Iso' (Angle v) v
deg = iso (\(Radians r) -> r ^/ (2*pi/360)) (Radians . (^*(2*pi/360)))

-- | An angle representing one full turn.
fullTurn :: (VectorSpace v, Floating (Scalar v), Num v) => Angle v
fullTurn = 1 @@ turn

-- | Calculate ratio between two angles.
angleRatio :: (InnerSpace v, Floating (Scalar v)) => Angle v -> Angle v -> Scalar v
angleRatio a b = (magnitude a) / (magnitude b)

-- | The sine of the given @Angle@.
sinA :: (Floating v) => Angle v -> v
sinA (Radians r) = sin r

-- | The cosine of the given @Angle@.
cosA :: (Floating v) => Angle v -> v
cosA (Radians r) = cos r

-- | The tangent function of the given @Angle@.
tanA :: (Floating v) => Angle v -> v
tanA (Radians r) = tan r

-- | The @Angle@ with the given sine.
asinA :: (Floating v) => v -> Angle v
asinA = Radians . asin

-- | The @Angle@ with the given cosine.
acosA :: (Floating v) => v -> Angle v
acosA = Radians . acos

-- | The @Angle@ with the given tangent.
atanA :: (Floating v) => v -> Angle v
atanA = Radians . atan

-- | @atan2A n d@ is the @Angle with tangent @n/d@, unless d is 0, in
-- which case it is ±π/2.
atan2A :: (RealFloat v) => v -> v -> Angle v
atan2A n d = Radians $ atan2 n d

-- | @30 \@\@ deg@ is an @Angle@ of the given measure and units.
--
-- More generally, @\@\@@ reverses the @Iso\'@ on its right, and
-- applies the @Iso\'@ to the value on the left.  @Angle@s are the
-- motivating example where this order improves readability.
(@@) :: b -> Iso' a b -> a
-- The signature above is slightly specialized, in favor of readability
a @@ i = review i a

infixl 5 @@

-- | compute the positive angle between the two vectors in their common plane
-- | N.B.: currently discards the common plane information
angleBetween  :: (InnerSpace v, Floating (Scalar v)) => v -> v -> Angle (Scalar v)
angleBetween v1 v2 = acos (normalized v1 <.> normalized v2) @@ rad

------------------------------------------------------------
-- Polar Coordinates

-- | The class of types with at least one angle coordinate, called _theta.
class HasTheta t where
    _theta :: Lens' t (Angle (Scalar (V t)))

-- | The class of types with at least two angle coordinates, the
-- second called _phi.
class HasPhi t where
    _phi :: Lens' t (Angle (Scalar (V t)))

