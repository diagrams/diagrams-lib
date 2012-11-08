{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , ViewPatterns
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Vector
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Two-dimensional vectors.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Vector
       ( -- * Special 2D vectors
         unitX, unitY, unit_X, unit_Y

         -- * Converting between vectors and angles
       , direction, fromDirection,  e
       ) where

import Data.Basis
import Data.VectorSpace

import Diagrams.Coordinates
import Diagrams.TwoD.Types

-- | The unit vector in the positive X direction.
unitX :: (Num a, HasBasis a, a ~ Scalar a, Basis a ~ ()) => D2 a
unitX = basisValue (Left ())

-- | The unit vector in the positive Y direction.
unitY :: (Num a, HasBasis a, a ~ Scalar a, Basis a ~ ()) => D2 a
unitY = basisValue (Right ())

-- | The unit vector in the negative X direction.
unit_X :: (Num a, AdditiveGroup (D2 a), HasBasis a, a ~ Scalar a, Basis a ~ ()) => D2 a
unit_X = negateV unitX

-- | The unit vector in the negative Y direction.
unit_Y :: (Num a, AdditiveGroup (D2 a), HasBasis a, a ~ Scalar a, Basis a ~ ()) => D2 a
unit_Y = negateV unitY

-- | Compute the direction of a vector, measured counterclockwise from
--   the positive x-axis as a fraction of a full turn.  The zero
--   vector is arbitrarily assigned the direction 0.
direction :: (RealFloat b, Angle a) => D2 b -> a
direction (coords -> x :& y) = convertAngle . Rad $ (uncurry encodeFloat).decodeFloat $ atan2 y x

-- | Convert an angle into a unit vector pointing in that direction.
fromDirection :: (RealFloat b, Angle a) => a -> D2 b
fromDirection a = ((uncurry encodeFloat).decodeFloat.cos) a' 
                & ((uncurry encodeFloat).decodeFloat.sin) a'
  where Rad a' = convertAngle a

-- | A convenient synonym for 'fromDirection'.
e :: (RealFloat b, Angle a) => a -> D2 b
e = fromDirection