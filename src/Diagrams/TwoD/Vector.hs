
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
       , e, xDir

         -- * 2D vector utilities
       , perp, leftTurn
         -- * Synonym for R2 things
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

-- | The origin of the direction AffineSpace.  For all d, @d .-. xDir
-- = d^._theta@.
xDir :: (R1 v, Additive v, Num n) => Direction v n
xDir = direction unitX

-- | A unit vector at a specified angle counterclockwise from the
-- positive X axis.
e :: Floating n => Angle n -> V2 n
e = angle . view rad

-- | @leftTurn v1 v2@ tests whether the direction of @v2@ is a left
--   turn from @v1@ (that is, if the direction of @v2@ can be obtained
--   from that of @v1@ by adding an angle 0 <= theta <= tau/2).
leftTurn :: (Num n, Ord n) => V2 n -> V2 n -> Bool
leftTurn v1 v2 = (v1 `dot` perp v2) < 0

