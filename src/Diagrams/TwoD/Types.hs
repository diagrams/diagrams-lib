{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

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
         R2(..), r2, unr2, mkR2, r2Iso
       , P2, p2, mkP2, unp2, p2Iso
       , T2

       ) where

import           Control.Lens            (Iso', Wrapped(..), Rewrapped, iso
                                         , (^.), _1, _2, lens)

import           Diagrams.Angle
import           Diagrams.Coordinates
import           Diagrams.Core

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Basis
import           Data.MemoTrie           (HasTrie (..))
import           Data.VectorSpace

import           Data.Typeable
------------------------------------------------------------
-- 2D Euclidean space

-- | The two-dimensional Euclidean vector space R^2.  This type is
--   intentionally abstract.
--
--   * To construct a vector, use 'r2', or '^&' (from "Diagrams.Coordinates"):
--
-- @
-- r2 (3,4) :: R2
-- 3 ^& 4    :: R2
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

data R2 = R2 {-# UNPACK #-} !Double
             {-# UNPACK #-} !Double
  deriving (Eq, Ord, Typeable)

instance AdditiveGroup R2 where
  zeroV = R2 0 0
  R2 x1 y1 ^+^ R2 x2 y2 = R2 (x1 + x2) (y1 + y2)
  negateV (R2 x y) = R2 (-x) (-y)

instance Num R2 where
  (+)                 = (^+^)
  R2 x1 y1 * R2 x2 y2 = R2 (x1 * x2) (y1 * y2)  -- this is sort of bogus
  (-)                 = (^-^)
  negate              = negateV
  abs (R2 x y)        = R2 (abs x) (abs y)
  signum (R2 x y)     = R2 (signum x) (signum y)
  fromInteger i       = R2 i' i'
    where i' = fromInteger i

instance Fractional R2 where
  R2 x1 y1 / R2 x2 y2 = R2 (x1/x2) (y1/y2)
  recip (R2 x y) = R2 (recip x) (recip y)
  fromRational r = R2 r' r'
    where r' = fromRational r

instance Show R2 where
  showsPrec p (R2 x y) = showParen (p >= 7) $
    showCoord x . showString " ^& " . showCoord y
   where
    showCoord c | c < 0     = showParen True (shows c)
                | otherwise = shows c

instance Read R2 where
  readsPrec d r = readParen (d > app_prec)
                  (\rr -> [ (R2 x y, r''')
                          | (x,r')    <- readsPrec (amp_prec + 1) rr
                          , ("^&",r'') <- lex r'
                          , (y,r''')  <- readsPrec (amp_prec + 1) r''
                          ])
                  r
    where
      app_prec = 10
      amp_prec = 7

-- | Construct a 2D vector from a pair of components.  See also '&'.
r2 :: (Double, Double) -> R2
r2 (x,y) = R2 x y

-- | Convert a 2D vector back into a pair of components.  See also 'coords'.
unr2 :: R2 -> (Double, Double)
unr2 (R2 x y) = (x,y)

-- | Curried form of `r2`.
mkR2 :: Double -> Double -> R2
mkR2 = curry r2

-- | Lens wrapped isomorphisms for R2.
instance Wrapped R2 where
    type Unwrapped R2 = (Double, Double)
    _Wrapped' = iso unr2 r2
    {-# INLINE _Wrapped' #-}

instance Rewrapped R2 R2

type instance V R2 = R2

instance VectorSpace R2 where
  type Scalar R2 = Double
  s *^ R2 x y = R2 (s*x) (s*y)

data R2Basis = XB | YB deriving (Eq, Ord, Enum)

instance HasTrie R2Basis where
    data R2Basis :->: x = R2Trie x x
    trie f = R2Trie (f XB) (f YB)
    untrie (R2Trie x _y) XB = x
    untrie (R2Trie _x y) YB = y
    enumerate (R2Trie x y)  = [(XB,x),(YB,y)]

instance HasBasis R2 where
  type Basis R2 = R2Basis
  basisValue XB          = R2 1 0
  basisValue YB          = R2 0 1

  decompose (R2 x y)             = [(XB, x), (YB, y)]

  decompose' (R2 x _) (XB)  = x
  decompose' (R2 _ y) (YB) = y

instance InnerSpace R2 where
  (R2 x1 y1) <.> (R2 x2 y2) = x1*x2 + y1*y2

instance Coordinates R2 where
  type FinalCoord R2     = Double
  type PrevDim R2        = Double
  type Decomposition R2  = Double :& Double

  x ^& y           = R2 x y
  coords (R2 x y) = x :& y

r2Iso :: Iso' R2 (Double, Double)
r2Iso = iso unr2 r2

instance HasX R2 where
    _x = r2Iso . _1

instance HasY R2 where
    _y = r2Iso . _2

instance HasTheta R2 where
    _theta = lens (\v -> atanA (v^._y / v^._x))
      (\v θ -> let r = magnitude v in R2 (r * cosA θ) (r * sinA θ))

instance HasR R2 where
    _r = lens magnitude (\v r -> let s = r/magnitude v in s *^ v)

-- | Points in R^2.  This type is intentionally abstract.
--
--   * To construct a point, use 'p2', or '^&' (see
--     "Diagrams.Coordinates"):
--
-- @
-- p2 (3,4)  :: P2
-- 3 ^& 4    :: P2
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

-- | Construct a 2D point from a pair of coordinates.  See also '^&'.
p2 :: (Double, Double) -> P2
p2 = P . r2

-- | Convert a 2D point back into a pair of coordinates.  See also 'coords'.
unp2 :: P2 -> (Double, Double)
unp2 (P v) = unr2 v

-- | Curried form of `p2`.
mkP2 :: Double -> Double -> P2
mkP2 = curry p2

-- | Transformations in R^2.
type T2 = Transformation R2

instance Transformable R2 where
  transform = apply

p2Iso :: Iso' P2 (Double, Double)
p2Iso = iso unp2 p2

instance HasX P2 where
    _x = p2Iso . _1

instance HasY P2 where
    _y = p2Iso . _2

instance HasR P2 where
    _r = _relative origin . _r

instance HasTheta P2 where
    _theta = _relative origin . _theta
