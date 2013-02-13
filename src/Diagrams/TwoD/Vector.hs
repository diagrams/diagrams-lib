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
unitX :: (Num a) => V2 a
unitX = 1 & 0 --basisValue (Left ())

-- | The unit vector in the positive Y direction.
unitY :: (Num a) => V2 a
unitY = 0 & 1

-- | The unit vector in the negative X direction.
unit_X :: (Num a, AdditiveGroup a) => V2 a
unit_X = negateV unitX

-- | The unit vector in the negative Y direction.
unit_Y :: (Num a, AdditiveGroup a) => V2 a
unit_Y = negateV unitY

-- | Compute the direction of a vector, measured counterclockwise from
--   the positive x-axis as a fraction of a full turn.  The zero
--   vector is arbitrarily assigned the direction 0.
direction :: (RealFloat a, Angle m a) => V2 a -> m a
direction (coords -> x :& y) = convertAngle . Rad $ atan2 y x

-- | Convert an angle into a unit vector pointing in that direction.
fromDirection :: (Floating a, Angle m a) => m a -> V2 a
fromDirection a = cos a' & sin a'
  where Rad a' = convertAngle a

-- | A convenient synonym for 'fromDirection'.
e :: (Floating a, Angle m a) => m a -> V2 a
e = fromDirection