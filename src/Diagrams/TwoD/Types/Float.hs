{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Types.Float
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- 2D Euclidean space in Float precision
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Types.Float where

import           Control.Lens         (Rewrapped, Wrapped (..), iso, (^.), _1, _2)


import           Diagrams.Angle
import           Diagrams.Coordinates
import           Diagrams.Core
import           Diagrams.TwoD.Types

import           Data.Basis
import           Data.VectorSpace

import           Data.Data


-- Orphan instances that should be in diagrams-core
type instance V Float = Float

instance Transformable Float where
  transform = apply

------------------------------------------------------------
-- 2D Euclidean space

-- | The two-dimensional Euclidean vector space R^2.  This type is
--   intentionally abstract, but uses Floats as the scalar type.
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
data R2 = R2 {-# UNPACK #-} !Float
             {-# UNPACK #-} !Float
  deriving (Eq, Ord, Typeable, Data)

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

-- | Lens wrapped isomorphisms for R2.
instance Wrapped R2 where
    type Unwrapped R2 = (Float, Float)
    _Wrapped' = iso unr2 r2
    {-# INLINE _Wrapped' #-}

instance Rewrapped R2 R2

type instance V R2 = R2

instance VectorSpace R2 where
  type Scalar R2 = Float
  s *^ R2 x y = R2 (s*x) (s*y)

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
  type FinalCoord R2     = Float
  type PrevDim R2        = Float
  type Decomposition R2  = Float :& Float

  x ^& y           = R2 x y
  coords (R2 x y) = x :& y

instance HasX R2 where
    _x = r2Iso . _1

instance HasY R2 where
    _y = r2Iso . _2

instance HasTheta R2 where
    _theta = polar._2

instance HasR R2 where
    _r = polar._1

instance Polar R2 where
    polar =
        iso (\v -> ( magnitude v, atan2A (v^._y) (v^._x)))
            (\(r,θ) -> R2 (r * cosA θ) (r * sinA θ))

instance Transformable R2 where
  transform = apply

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

-- | Transformations in R^2.
type T2 = Transformation R2
