{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

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
         R2, r2, unr2
       , P2, p2, unp2
       , T2

         -- * Angles
       , Angle(..)
       , Turn(..), CircleFrac, Rad(..), Deg(..)
       , fullCircle, convertAngle
       ) where

import           Diagrams.Coordinates
import           Diagrams.Core
import           Diagrams.Util           (tau)

import           Control.Newtype

import           Data.Basis
import           Data.VectorSpace

import           Data.Typeable

------------------------------------------------------------
-- 2D Euclidean space

-- | The two-dimensional Euclidean vector space R^2.  This type is
--   intentionally abstract.
--
--   * To construct a vector, use 'r2', or '&' (from "Diagrams.Coordinates"):
--
-- @
-- r2 (3,4) :: R2
-- 3 & 4    :: R2
-- @
--
--     Note that "Diagrams.Coordinates" is not re-exported by
--     "Diagrams.Prelude" and must be explicitly imported.
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
-- @
-- foo (unr2 -> (x,y)) = ...
-- foo (coords -> x :& y) = ...
-- @

newtype R2 = R2 { unR2 :: (Double, Double) }
  deriving (AdditiveGroup, Eq, Ord, Typeable)

instance Show R2 where
  showsPrec p (R2 (x,y)) = showParen (p >= 7) $
    showCoord x . showString " & " . showCoord y
   where
    showCoord x | x < 0     = showParen True (shows x)
                | otherwise = shows x

instance Read R2 where
  readsPrec d r = readParen (d > app_prec)
                  (\r -> [ (R2 (x,y), r''')
                         | (x,r')    <- readsPrec (amp_prec + 1) r
                         , ("&",r'') <- lex r'
                         , (y,r''')  <- readsPrec (amp_prec + 1) r''
                         ])
                  r
    where
      app_prec = 10
      amp_prec = 7

instance Newtype R2 (Double, Double) where
  pack   = R2
  unpack = unR2

-- | Construct a 2D vector from a pair of components.  See also '&'.
r2 :: (Double, Double) -> R2
r2 = pack

-- | Convert a 2D vector back into a pair of components.  See also 'coords'.
unr2 :: R2 -> (Double, Double)
unr2 = unpack

type instance V R2 = R2

instance VectorSpace R2 where
  type Scalar R2 = Double
  (*^) = over R2 . (*^)

instance HasBasis R2 where
  type Basis R2 = Either () () -- = Basis (Double, Double)
  basisValue = R2 . basisValue
  decompose  = decompose  . unR2
  decompose' = decompose' . unR2

instance InnerSpace R2 where
  (unR2 -> vec1) <.> (unR2 -> vec2) = vec1 <.> vec2

instance Coordinates R2 where
  type FinalCoord R2     = Double
  type PrevDim R2        = Double
  type Decomposition R2  = Double :& Double

  x & y                  = r2 (x,y)
  coords (unR2 -> (x,y)) = x :& y

-- | Points in R^2.  This type is intentionally abstract.
--
--   * To construct a point, use 'p2', or '&' (see
--     "Diagrams.Coordinates"):
--
-- @
-- p2 (3,4)  :: P2
-- 3 & 4     :: P2
-- @
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
-- @
-- foo (unp2 -> (x,y)) = ...
-- foo (coords -> x :& y) = ...
-- @
type P2 = Point R2

-- | Construct a 2D point from a pair of coordinates.  See also '&'.
p2 :: (Double, Double) -> P2
p2 = pack . pack

-- | Convert a 2D point back into a pair of coordinates.  See also 'coords'.
unp2 :: P2 -> (Double, Double)
unp2 = unpack . unpack

-- | Transformations in R^2.
type T2 = Transformation R2

instance Transformable R2 where
  transform = apply

------------------------------------------------------------
-- Angles

-- | Newtype wrapper used to represent angles as fractions of a
--   circle.  For example, 1\/3 turn = tau\/3 radians = 120 degrees.
newtype Turn = Turn { getTurn :: Double }
  deriving (Read, Show, Eq, Ord, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

-- | Deprecated synonym for 'Turn', retained for backwards compatibility.
type CircleFrac = Turn

-- | Newtype wrapper for representing angles in radians.
newtype Rad = Rad { getRad :: Double }
  deriving (Read, Show, Eq, Ord, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

-- | Newtype wrapper for representing angles in degrees.
newtype Deg = Deg { getDeg :: Double }
  deriving (Read, Show, Eq, Ord, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

-- | Type class for types that measure angles.
class Num a => Angle a where
  -- | Convert to a turn, /i.e./ a fraction of a circle.
  toTurn   :: a -> Turn

  -- | Convert from a turn, /i.e./ a fraction of a circle.
  fromTurn :: Turn -> a

instance Angle Turn where
  toTurn   = id
  fromTurn = id

-- | tau radians = 1 full turn.
instance Angle Rad where
  toTurn   = Turn . (/tau) . getRad
  fromTurn = Rad . (*tau) . getTurn

-- | 360 degrees = 1 full turn.
instance Angle Deg where
  toTurn   = Turn . (/360) . getDeg
  fromTurn = Deg . (*360) . getTurn

-- | An angle representing one full turn.
fullTurn :: Angle a => a
fullTurn = fromTurn 1

-- | Deprecated synonym for 'fullTurn', retained for backwards compatibility.
fullCircle :: Angle a => a
fullCircle = fullTurn

-- | Convert between two angle representations.
convertAngle :: (Angle a, Angle b) => a -> b
convertAngle = fromTurn . toTurn
