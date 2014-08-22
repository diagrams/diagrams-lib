{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Types.Generic
-- Copyright   :  (c) 2014 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Generic type for two-dimensional Euclidean space.
-----------------------------------------------------------------------------

module Diagrams.TwoD.Types.Generic where

import           Control.Lens         (Rewrapped, Wrapped (..), iso, (^.), _1, _2)


import           Diagrams.Angle
import           Diagrams.Coordinates
import           Diagrams.Core
import           Diagrams.TwoD.Types

import           Data.Basis
import           Data.VectorSpace

import           Data.Data
import           Data.Foldable
import           Data.Traversable

data V2 a = V2 a a
  deriving (Eq, Typeable, Functor, Foldable, Traversable, Data)

instance (ScalarTwoD a) => AdditiveGroup (V2 a) where
  zeroV = V2 0 0
  V2 x1 y1 ^+^ V2 x2 y2 = V2 (x1 + x2) (y1 + y2)
  negateV (V2 x y) = V2 (-x) (-y)

instance (ScalarTwoD a) => Num (V2 a) where
  (+)                 = (^+^)
  V2 x1 y1 * V2 x2 y2 = V2 (x1 * x2) (y1 * y2)  -- this is sort of bogus
  (-)                 = (^-^)
  negate              = negateV
  abs (V2 x y)        = V2 (abs x) (abs y)
  signum (V2 x y)     = V2 (signum x) (signum y)
  fromInteger i       = V2 i' i'
    where i' = fromInteger i

instance (ScalarTwoD a) => Fractional (V2 a) where
  V2 x1 y1 / V2 x2 y2 = V2 (x1/x2) (y1/y2)
  recip (V2 x y) = V2 (recip x) (recip y)
  fromRational r = V2 r' r'
    where r' = fromRational r

instance (ScalarTwoD a, Show a) => Show (V2 a) where
  showsPrec p (V2 x y) = showParen (p >= 7) $
    showCoord x . showString " ^& " . showCoord y
   where
    showCoord = showParen True . shows

-- | Lens wrapped isomorphisms for V2.
instance (ScalarTwoD a) => Wrapped (V2 a) where
    type Unwrapped (V2 a) = (a, a)
    _Wrapped' = iso unr2 r2
    {-# INLINE _Wrapped' #-}

instance (ScalarTwoD a) => Rewrapped (V2 a) (V2 a)

type instance V (V2 a) = V2 a

instance (ScalarTwoD a) => VectorSpace (V2 a) where
  type Scalar (V2 a) = a
  s *^ V2 x y = V2 (s*x) (s*y)

instance (ScalarTwoD a) => HasBasis (V2 a) where
  type Basis (V2 a) = R2Basis
  basisValue XB          = V2 1 0
  basisValue YB          = V2 0 1

  decompose (V2 x y)             = [(XB, x), (YB, y)]

  decompose' (V2 x _) (XB)  = x
  decompose' (V2 _ y) (YB) = y

instance (ScalarTwoD a) => InnerSpace (V2 a) where
  (V2 x1 y1) <.> (V2 x2 y2) = x1*x2 + y1*y2

instance (ScalarTwoD a) => Coordinates (V2 a) where
  type FinalCoord (V2 a)    = a
  type PrevDim (V2 a)       = a
  type Decomposition (V2 a) = a :& a

  x ^& y           = V2 x y
  coords (V2 x y) = x :& y

instance (ScalarTwoD a) => HasX (V2 a) where
    _x = r2Iso . _1

instance (ScalarTwoD a) => HasY (V2 a) where
    _y = r2Iso . _2

instance (ScalarTwoD a) => HasTheta (V2 a) where
    _theta = polar._2

instance (ScalarTwoD a) => HasR (V2 a) where
    _r = polar._1

instance (ScalarTwoD a) => Polar (V2 a) where
    polar =
        iso (\v -> ( magnitude v, atan2A (v^._y) (v^._x)))
            (\(r,θ) -> V2 (r * cosA θ) (r * sinA θ))

instance (ScalarTwoD a) => Transformable (V2 a) where
  transform = apply

