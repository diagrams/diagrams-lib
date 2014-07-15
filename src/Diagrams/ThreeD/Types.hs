{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Types
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Basic types for three-dimensional Euclidean space.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Types
       ( -- * 3D Euclidean space
         R3(..), r3, unr3, mkR3
       , P3, p3, unp3, mkP3
       , T3
       , r3Iso, p3Iso
       ) where

import           Control.Lens           (Iso', iso, lens, over, to
                                        , _1, _2, _3)

import           Diagrams.Core
import           Diagrams.Angle
import           Diagrams.Direction
import           Diagrams.TwoD.Types    (R2)
import           Diagrams.Coordinates

import           Data.AffineSpace.Point
import           Data.Basis
import           Data.Cross
import           Data.VectorSpace

------------------------------------------------------------
-- 3D Euclidean space

-- | The three-dimensional Euclidean vector space R^3.
data R3 = R3 !Double !Double !Double
  deriving (Eq, Ord, Show, Read)

r3Iso :: Iso' R3 (Double, Double, Double)
r3Iso = iso unr3 r3

-- | Construct a 3D vector from a triple of components.
r3 :: (Double, Double, Double) -> R3
r3 (x,y,z) = R3 x y z

-- | Curried version of `r3`.
mkR3 :: Double -> Double -> Double -> R3
mkR3 = R3

-- | Convert a 3D vector back into a triple of components.
unr3 :: R3 -> (Double, Double, Double)
unr3 (R3 x y z) = (x,y,z)

instance AdditiveGroup R3 where
    zeroV = R3 0 0 0
    R3 x1 y1 z1 ^+^ R3 x2 y2 z2 = R3 (x1 + x2) (y1 + y2) (z1 + z2)
    negateV (R3 x y z) = R3 (-x) (-y) (-z)

type instance V R3 = R3

instance VectorSpace R3 where
  type Scalar R3 = Double
  (*^) = over r3Iso . (*^)

instance HasBasis R3 where
  type Basis R3 = Either () (Either () ()) -- = Basis (Double, Double, Double)
  basisValue = r3 . basisValue
  decompose  = decompose  . unr3
  decompose' = decompose' . unr3

instance InnerSpace R3 where
    (R3 x1 y1 z1) <.> (R3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

instance Coordinates R3 where
  type FinalCoord R3       = Double
  type PrevDim R3          = R2
  type Decomposition R3    = Double :& Double :& Double

  (coords -> x :& y) ^& z   = R3 x y z
  coords (R3 x y z) = x :& y :& z

-- | Points in R^3.
type P3 = Point R3

-- | Construct a 3D point from a triple of coordinates.
p3 :: (Double, Double, Double) -> P3
p3 = P . r3

-- | Convert a 3D point back into a triple of coordinates.
unp3 :: P3 -> (Double, Double, Double)
unp3 = unr3 . unPoint

p3Iso :: Iso' P3 (Double, Double, Double)
p3Iso = iso unp3 p3

-- | Curried version of `r3`.
mkP3 :: Double -> Double -> Double -> P3
mkP3 x y z = p3 (x, y, z)

-- | Transformations in R^3.
type T3 = Transformation R3

instance Transformable R3 where
  transform = apply

instance HasCross3 R3 where
  cross3 u v = r3 $ cross3 (unr3 u) (unr3 v)

instance HasX R3 where
    _x = r3Iso . _1

instance HasX P3 where
    _x = p3Iso . _1

instance HasY R3 where
    _y = r3Iso . _2

instance HasY P3 where
    _y = p3Iso . _2

instance HasZ R3 where
    _z = r3Iso . _3

instance HasZ P3 where
    _z = p3Iso . _3

instance HasR R3 where
    _r = lens magnitude (\v r -> v ^* (r / magnitude v))

instance HasR P3 where
    _r = _relative origin . _r

instance HasTheta R3 where
    _theta = to $ \(R3 x y _) -> atan2A y x

instance HasTheta P3 where
    _theta = _relative origin . _theta

instance HasTheta (Direction R3) where
    _theta = _Dir . _theta
