{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DeriveFunctor       #-}
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
       , fullTurn, halfTurn, quarterTurn, angleRatio
       , sinA, cosA, tanA, asinA, acosA, atanA, atan2A
       , (@@)
       , angleBetween
       , HasTheta(..)
       , HasPhi(..)
       ) where

import           Control.Lens     (Iso', Lens', iso, review, (^.))

import           Data.Monoid      hiding ((<>))
import           Data.Semigroup
-- import           Data.VectorSpace
import Linear.Affine
import Linear.Metric
import Linear.Vector
import Linear.Epsilon

import Control.Applicative

import           Diagrams.Core.V
import           Diagrams.Points
-- import Data.Fixed

-- | Angles can be expressed in a variety of units.  Internally,
-- they are represented in radians.
newtype Angle n = Radians n
  deriving (Read, Show, Eq, Ord, Functor)

instance Applicative Angle where
  pure = Radians
  {-# INLINE pure #-}
  Radians f <*> Radians x = Radians (f x)
  {-# INLINE (<*>) #-}

instance Additive Angle where
  zero = pure 0
  {-# INLINE zero #-}

instance Num n => Semigroup (Angle n) where
  (<>) = (^+^)

instance Num n => Monoid (Angle n) where
  mappend = (<>)
  mempty  = Radians 0

-- deriving instance InnerSpace v => InnerSpace (Angle v)

type instance N (Angle n) = n

-- | The radian measure of an @Angle@ @a@ can be accessed as @a
-- ^. rad@.  A new @Angle@ can be defined in radians as @pi \@\@ rad@.
rad :: Iso' (Angle n) n
rad = iso (\(Radians r) -> r) Radians

-- | The measure of an @Angle@ @a@ in full circles can be accessed as
-- @a ^. turn@.  A new @Angle@ of one-half circle can be defined in as
-- @1/2 \@\@ turn@.
turn :: Floating n => Iso' (Angle n) n
turn = iso (\(Radians r) -> r / (2*pi)) (Radians . (*(2*pi)))

-- | The degree measure of an @Angle@ @a@ can be accessed as @a
-- ^. deg@.  A new @Angle@ can be defined in degrees as @180 \@\@
-- deg@.
deg :: Floating n => Iso' (Angle n) n
deg = iso (\(Radians r) -> r / (2*pi/360)) (Radians . ( * (2*pi/360)))

-- | An angle representing one full turn.
fullTurn :: Floating v => Angle v
fullTurn = 1 @@ turn

-- | An angle representing a half.
halfTurn :: Floating v => Angle v
halfTurn = 0.5 @@ turn

-- | An angle representing a quarter turn.
quarterTurn :: Floating v => Angle v
quarterTurn = 0.25 @@ turn

-- | Calculate ratio between two angles.
angleRatio :: Floating n => Angle n -> Angle n -> n
angleRatio a b = abs (a ^. rad) / abs (b ^. rad)

-- | The sine of the given @Angle@.
sinA :: Floating n => Angle n -> n
sinA (Radians r) = sin r

-- | The cosine of the given @Angle@.
cosA :: Floating n => Angle n -> n
cosA (Radians r) = cos r

-- | The tangent function of the given @Angle@.
tanA :: Floating n => Angle n -> n
tanA (Radians r) = tan r

-- | The @Angle@ with the given sine.
asinA :: Floating n => n -> Angle n
asinA = Radians . asin

-- | The @Angle@ with the given cosine.
acosA :: Floating n => n -> Angle n
acosA = Radians . acos

-- | The @Angle@ with the given tangent.
atanA :: Floating n => n -> Angle n
atanA = Radians . atan

-- | @atan2A n d@ is the @Angle with tangent @n/d@, unless d is 0, in
-- which case it is ±π/2.
atan2A :: RealFloat n => n -> n -> Angle n
atan2A y x = Radians $ atan2 y x

-- Like atan2 but unable to differentiate between 0 and -0:
-- atan2 0 (-0)  = pi
-- atan2' 0 (-0) = 0
--
-- have to decide if this technicality is worth a RealFloat instance.

-- atan2' :: (Floating n, Ord n) => n -> n -> n
-- atan2' y x
--   | x > 0            =  atan (y/x)
--   | x == 0 && y >  0 =  pi/2
--   | x <  0 && y >  0 =  pi + atan (y/x)
--   | x <= 0 && y <  0 = -atan2' (-y) x
--   | y == 0 && x <  0 =  pi    -- must be after the previous test on zero y
--   | x == 0 && y == 0 =  y     -- must be after the other double zero tests
--   | otherwise        =  x + y -- x or y is a NaN, return a NaN (via +)

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
angleBetween  :: (Metric v, Floating n, Epsilon n) => v n -> v n -> Angle n
angleBetween v1 v2 = acos (normalize v1 `dot` normalize v2) @@ rad

-- | Normalize an angle so that is lies in the [0,tau) range.
-- normalizeAngle :: (Floating n, Real n) => Angle n -> Angle n
-- normalizeAngle = over rad (`mod'` (2 * pi))

------------------------------------------------------------
-- Polar Coordinates

-- | The class of types with at least one angle coordinate, called _theta.
class HasTheta t where
    _theta :: RealFloat n => Lens' (t n) (Angle n)

-- | The class of types with at least two angle coordinates, the
-- second called _phi.
class HasTheta t => HasPhi t where
    _phi :: RealFloat n => Lens' (t n) (Angle n)

-- Point instances
instance HasTheta v => HasTheta (Point v) where
    _theta = _pIso . _theta

instance HasPhi v => HasPhi (Point v) where
    _phi = _pIso . _phi

