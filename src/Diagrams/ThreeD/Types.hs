{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
         R3, r3, unr3
       , P3, p3, unp3
       , T3

       ) where

import Diagrams.Coordinates
import Diagrams.TwoD.Types
import Diagrams.Core

import Control.Newtype

import Data.Basis
import Data.VectorSpace
import Data.Cross

------------------------------------------------------------
-- 3D Euclidean space

-- | The three-dimensional Euclidean vector space R^3.
newtype R3 = R3 { unR3 :: (Double, Double, Double) }
  deriving (AdditiveGroup, Eq, Ord, Show, Read)

instance Newtype R3 (Double, Double, Double) where
  pack   = R3
  unpack = unR3

-- | Construct a 3D vector from a triple of components.
r3 :: (Double, Double, Double) -> R3
r3 = pack

-- | Convert a 3D vector back into a triple of components.
unr3 :: R3 -> (Double, Double, Double)
unr3 = unpack

type instance V R3 = R3

instance VectorSpace R3 where
  type Scalar R3 = Double
  (*^) = over R3 . (*^)

instance HasBasis R3 where
  type Basis R3 = Either () (Either () ()) -- = Basis (Double, Double, Double)
  basisValue = R3 . basisValue
  decompose  = decompose  . unR3
  decompose' = decompose' . unR3

instance InnerSpace R3 where
  (unR3 -> vec1) <.> (unR3 -> vec2) = vec1 <.> vec2

instance Coordinates R3 where
  type FinalCoord R3       = Double
  type PrevDim R3          = R2
  type Decomposition R3    = Double :& Double :& Double

  (coords -> x :& y) & z   = r3 (x,y,z)
  coords (unR3 -> (x,y,z)) = x :& y :& z

-- | Points in R^3.
type P3 = Point R3

-- | Construct a 3D point from a triple of coordinates.
p3 :: (Double, Double, Double) -> P3
p3 = pack . pack

-- | Convert a 2D point back into a triple of coordinates.
unp3 :: P3 -> (Double, Double, Double)
unp3 = unpack . unpack

-- | Transformations in R^3.
type T3 = Transformation R3

instance Transformable R3 where
  transform = apply

instance HasCross3 R3 where
  cross3 u v = r3 $ cross3 (unr3 u) (unr3 v)
