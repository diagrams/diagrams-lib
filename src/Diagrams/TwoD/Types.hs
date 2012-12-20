{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE UndecidableInstances #-}

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
         R2
       , r2, unr2
       , P2, P2D
       , p2, unp2
       , V2
       , v2, unv2
       , T2, T2D
       -- * Angles
       , Angle(..)
       , CircleFrac(..), Rad(..), Deg(..)
       , fullCircle, convertAngle
       ) where

import Diagrams.Coordinates
import Diagrams.Util (tau)
import Diagrams.Core

import Control.Newtype

import Data.Basis
import Data.NumInstances ()
import Data.VectorSpace

import Data.Typeable

------------------------------------------------------------
-- 2D Euclidean space

-- | The two-dimensional Euclidean vector space R^2.  This type is
--   intentionally abstract.
--
--   * To construct a vector, use 'r2', or '&' (from "Diagrams.Coordinates"):
--
-- > r2 (3,4) :: R2
-- > 3 & 4    :: R2
--
--   * To construct the vector from the origin to a point @p@, use
--     @p 'Data.AffineSpace..-.' 'origin'@.
--
--   * To convert a vector @v@ into the point obtained by following
--     @v@ from the origin, use @'origin' 'Data.AffineSpace..+^' v@.
--
--   * To convert a vector back into a pair of components, use 'unv2'
--     or 'coords' (from "Diagrams.Coordinates").  These are typically
--     used in conjunction with the @ViewPatterns@ extension:
--
-- > foo (unr2 -> (x,y)) = ...
-- > foo (coords -> x :& y) = ...

newtype V2 a = V2 { unV2 :: (a, a) }
  deriving (AdditiveGroup, Eq, Ord, Typeable, Num, Fractional)

v2 :: (a, a) -> V2 a
v2 = V2

unv2 :: V2 a -> (a, a)
unv2 = unpack

type R2 = V2 Double

unR2 :: R2 -> (Double, Double)
unR2 = unV2

instance (Show a, Num a, Ord a) => Show (V2 a) where
  showsPrec p (V2 (x,y)) = showParen (p >= 7) $
    showCoord x . showString " & " . showCoord y
   where
    showCoord x | x < 0     = showParen True (shows x)
                | otherwise = shows x

instance (Read a) => Read (V2 a) where
  readsPrec d r = readParen (d > app_prec)
                  (\r -> [ (V2 (x,y), r''')
                         | (x,r')    <- readsPrec (amp_prec + 1) r
                         , ("&",r'') <- lex r'
                         , (y,r''')  <- readsPrec (amp_prec + 1) r''
                         ])
                  r
    where
      app_prec = 10
      amp_prec = 7

instance Newtype (V2 a) (a, a) where
  pack   = V2
  unpack = unV2

-- | Construct a 2D vector from a pair of components.  See also '&'.
r2 :: (Double, Double) -> R2
r2 = pack

-- | Convert a 2D vector back into a pair of components.  See also 'coords'.
unr2 :: R2 -> (Double, Double)
unr2 = unpack

type instance V (V2 a) = V2 a

instance (AdditiveGroup a, Num a) => VectorSpace (V2 a) where
  type Scalar (V2 a) = a
  s *^ v = let (vx, vy) = unv2 v
           in v2 (s * vx, s * vy)

-- GHC can't deduce "a ~ Scalar a", so it has to be added here.
-- This is why "UndecidableInstances" is needed.
instance (AdditiveGroup a, Num a, HasBasis a, a ~ Scalar a) => HasBasis (V2 a) where
  type Basis (V2 a) = Basis (a,a) -- should be equal to: Either (Basis a) (Basis a)
  basisValue = v2 . basisValue
  decompose  = decompose  . unv2
  decompose' = decompose' . unv2

instance (AdditiveGroup a, Num a, InnerSpace a, a ~ Scalar a) => InnerSpace (V2 a) where
  (unv2 -> vec1) <.> (unv2 -> vec2) = vec1 <.> vec2

instance Coordinates (V2 a) where
  type FinalCoord (V2 a)     = a
  type PrevDim (V2 a)        = a
  type Decomposition (V2 a)  = a :& a

  x & y                  = v2 (x,y)
  coords (unv2 -> (x,y)) = x :& y

-- | Points in R^2.  This type is intentionally abstract.
--
--   * To construct a point, use 'p2', or '&' (see
--     "Diagrams.Coordinates"):
--
-- > p2 (3,4)  :: P2
-- > 3 & 4     :: P2
--
--   * To construct a point from a vector @v@, use @'origin' 'Data.AffineSpace..+^' v@.
--
--   * To convert a point @p@ into the vector from the origin to @p@,
--   use @p 'Data.AffineSpace..-.' 'origin'@.
--
--   * To convert a point back into a pair of coordinates, use 'unp2',
--     or 'coords' (from "Diagrams.Coordinates").  It's common to use
--     these in conjunction with the @ViewPatterns@ extension:
--
-- > foo (unp2 -> (x,y)) = ...
-- > foo (coords -> x :& y) = ...
type P2 a = Point (V2 a)
type P2D = P2 Double

-- | Construct a 2D point from a pair of coordinates.  See also '&'.
p2 :: (a, a) -> P2 a
p2 = pack . pack

-- | Convert a 2D point back into a pair of coordinates.  See also 'coords'.
unp2 :: P2 a -> (a, a)
unp2 = unpack . unpack

-- | Transformations in R^2.
type T2 a = Transformation (V2 a)
type T2D = T2 Double

instance (HasLinearMap (V2 a)) => Transformable (V2 a) where
  transform = apply

------------------------------------------------------------
-- Angles

-- | Newtype wrapper used to represent angles as fractions of a
--   circle.  For example, 1\/3 = tau\/3 radians = 120 degrees.
newtype CircleFrac a = CircleFrac { getCircleFrac :: a }
  deriving (Read, Show, Eq, Ord, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

-- | Newtype wrapper for representing angles in radians.
newtype Rad a = Rad { getRad :: a }
  deriving (Read, Show, Eq, Ord, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

-- | Newtype wrapper for representing angles in degrees.
newtype Deg a = Deg { getDeg :: a }
  deriving (Read, Show, Eq, Ord, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

-- | Type class for types that measure angles.
class (Num a, Num (m a)) => Angle m a where
  -- | Convert to a fraction of a circle.
  toCircleFrac   :: m a -> CircleFrac a

  -- | Convert from a fraction of a circle.
  fromCircleFrac :: CircleFrac a -> m a

instance (Num a) => Angle CircleFrac a where
  toCircleFrac   = id
  fromCircleFrac = id

-- | tau radians = 1 full circle.
instance (Floating a) => Angle Rad a where
  toCircleFrac   = CircleFrac . (/tau) . getRad
  fromCircleFrac = Rad . (*tau) . getCircleFrac

-- | 360 degrees = 1 full circle.
instance (Fractional a) => Angle Deg a where
  toCircleFrac   = CircleFrac . (/360) . getDeg
  fromCircleFrac = Deg . (*360) . getCircleFrac

-- | An angle representing a full circle.
fullCircle :: Angle m a => m a
fullCircle = fromCircleFrac 1

-- | Convert between two angle representations.
convertAngle :: (Angle ma a, Angle mb a) => ma a -> mb a
convertAngle = fromCircleFrac . toCircleFrac