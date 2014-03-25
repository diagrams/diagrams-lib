{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
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
       , fullTurn, fullCircle, angleRatio
       , sinA, cosA, tanA, asinA, acosA, atanA
       , (@@)
       , angleBetween
       , HasTheta(..)
       ) where

import           Control.Lens            (Iso', Lens', iso, review, (^.))
                                         -- , review , (^.), _1, _2, Lens', lens)

import           Data.VectorSpace

-- | Angles can be expressed in a variety of units.  Internally,
-- they are represented in radians.
newtype Angle = Radians Double
              deriving (Read, Show, Eq, Ord, Enum, AdditiveGroup)

instance VectorSpace Angle where
  type Scalar Angle = Double
  s *^ Radians t = Radians (s*t)

-- | The radian measure of an @Angle@ @a@ can be accessed as @a
-- ^. rad@.  A new @Angle@ can be defined in radians as @pi \@\@ rad@.
rad :: Iso' Angle Double
rad = iso (\(Radians r) -> r) Radians

-- | The measure of an @Angle@ @a@ in full circles can be accessed as
-- @a ^. turn@.  A new @Angle@ of one-half circle can be defined in as
-- @1/2 \@\@ turn@.
turn :: Iso' Angle Double
turn = iso (\(Radians r) -> r/2/pi) (Radians . (*(2*pi)))

-- | The degree measure of an @Angle@ @a@ can be accessed as @a
-- ^. deg@.  A new @Angle@ can be defined in degrees as @180 \@\@
-- deg@.
deg :: Iso' Angle Double
deg = iso (\(Radians r) -> r/2/pi*360) (Radians . (*(2*pi/360)))

-- | An angle representing one full turn.
fullTurn :: Angle
fullTurn = 1 @@ turn

-- | Deprecated synonym for 'fullTurn', retained for backwards compatibility.
fullCircle :: Angle
fullCircle = fullTurn

-- | Calculate ratio between two angles.
angleRatio :: Angle -> Angle -> Double
angleRatio a b = (a^.rad) / (b^.rad)

-- | The sine of the given @Angle@.
sinA :: Angle -> Double
sinA (Radians r) = sin r

-- | The cosine of the given @Angle@.
cosA :: Angle -> Double
cosA (Radians r) = cos r

-- | The tangent function of the given @Angle@.
tanA :: Angle -> Double
tanA (Radians r) = tan r

-- | The @Angle@ with the given sine.
asinA :: Double -> Angle
asinA = Radians . asin

-- | The @Angle@ with the given cosine.
acosA :: Double -> Angle
acosA = Radians . acos

-- | The @Angle@ with the given tangent.
atanA :: Double -> Angle
atanA = Radians . atan

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
angleBetween  :: (InnerSpace v, Scalar v ~ Double) => v -> v -> Angle
angleBetween v1 v2 = acos (normalized v1 <.> normalized v2) @@ rad

------------------------------------------------------------
-- Polar Coordinates

-- | The class of types with at least one angle coordinate, called _theta.
class HasTheta t where
    _theta :: Lens' t Angle
