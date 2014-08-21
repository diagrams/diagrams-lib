
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
       , unit, unit_

         -- * Converting between vectors and angles
       , e, xDir

         -- * 2D vector utilities
       , perp, leftTurn
         -- * Synonym for R2 things
       ) where

import           Control.Lens        ((&), (.~), set', ASetter')

import           Diagrams.Angle
import           Diagrams.Direction

import Linear.Vector hiding (unit)
import Linear.V2 hiding (_x,_y)
import Linear.Metric
import Diagrams.Coordinates

unit :: (Additive v, Num n) => ASetter' (v n) n -> v n
unit l = set' l 1 zero

unit_ :: (Additive v, Num n) => ASetter' (v n) n -> v n
unit_ l = set' l (-1) zero


-- | The unit vector in the positive X direction.
unitX :: (HasX v, Additive v, Floating n) => v n
unitX = unit _x

-- | The unit vector in the positive Y direction.
unitY :: (HasY v, Additive v, Floating n) => v n
unitY = unit _y

-- | The unit vector in the negative X direction.
unit_X :: (HasX v, Additive v, Floating n) => v n
unit_X = unit_ _x

-- | The unit vector in the negative Y direction.
unit_Y :: (HasY v, Additive v, Floating n) => v n
unit_Y = unit_ _y

-- | The origin of the direction AffineSpace.  For all d, @d .-. xDir
-- = d^._theta@.
xDir :: (HasX v, Additive v, Floating n) => Direction v n
xDir = direction unitX

-- | A unit vector at a specified angle counterclockwise from the
-- positive X axis.
e :: (HasTheta v, HasX v, Additive v, RealFloat n) => Angle n -> v n
e a = unitX & _theta .~ a

-- -- | @perp v@ is perpendicular to and has the same magnitude as @v@.
-- --   In particular @perp v == rotateBy (1/4) v@.
-- perp :: Num n => V2 n -> V2 n
-- perp (V2 x y) = V2 (-y) x

-- | @leftTurn v1 v2@ tests whether the direction of @v2@ is a left
--   turn from @v1@ (that is, if the direction of @v2@ can be obtained
--   from that of @v1@ by adding an angle 0 <= theta <= tau/2).
leftTurn :: (Num n, Ord n) => V2 n -> V2 n -> Bool
leftTurn v1 v2 = (v1 `dot` perp v2) < 0

