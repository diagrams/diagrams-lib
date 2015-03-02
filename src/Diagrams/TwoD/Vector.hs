
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
       , xDir, yDir

         -- * Converting between vectors and angles
       , angleV, angleDir, e

         -- * 2D vector utilities
       , perp, leftTurn, cross2
       ) where

import           Control.Lens       (view, (&), (.~))

import           Diagrams.Angle
import           Diagrams.Direction

import           Linear.Metric
import           Linear.V2
import           Linear.Vector

-- | The unit vector in the positive X direction.
unitX :: (R1 v, Additive v, Num n) => v n
unitX = zero & _x .~ 1

-- | The unit vector in the negative X direction.
unit_X :: (R1 v, Additive v, Num n) => v n
unit_X = zero & _x .~ (-1)

-- | The unit vector in the positive Y direction.
unitY :: (R2 v, Additive v, Num n) => v n
unitY = zero & _y .~ 1

-- | The unit vector in the negative Y direction.
unit_Y :: (R2 v, Additive v, Num n) => v n
unit_Y = zero & _y .~ (-1)

-- | A 'Direction' pointing in the X direction.
xDir :: (R1 v, Additive v, Num n) => Direction v n
xDir = dir unitX

-- | A 'Direction' pointing in the Y direction.
yDir :: (R2 v, Additive v, Num n) => Direction v n
yDir = dir unitY

-- | A direction at a specified angle counter-clockwise from the 'xDir'.
angleDir :: Floating n => Angle n -> Direction V2 n
angleDir = dir . angleV

-- | A unit vector at a specified angle counter-clockwise from the
--   positive x-axis
angleV :: Floating n => Angle n -> V2 n
angleV = angle . view rad

-- | A unit vector at a specified angle counter-clockwise from the
--   positive X axis.
e :: Floating n => Angle n -> V2 n
e = angleV

-- | @leftTurn v1 v2@ tests whether the direction of @v2@ is a left
--   turn from @v1@ (that is, if the direction of @v2@ can be obtained
--   from that of @v1@ by adding an angle 0 <= theta <= tau/2).
leftTurn :: (Num n, Ord n) => V2 n -> V2 n -> Bool
leftTurn v1 v2 = (v1 `dot` perp v2) < 0

-- | Cross product on vectors in R2.
cross2 :: Num n => V2 n -> V2 n -> n
cross2 (V2 x1 y1) (V2 x2 y2) = x1 * y2 - y1 * x2

