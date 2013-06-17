{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}
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

         -- * 2D vector utilities
       , perp, leftTurn
       ) where

import           Data.VectorSpace     ((<.>))
import           Diagrams.Coordinates
import           Diagrams.TwoD.Types
import           Diagrams.Util        (( # ))

-- | The unit vector in the positive X direction.
unitX :: R2
unitX = 1 & 0

-- | The unit vector in the positive Y direction.
unitY :: R2
unitY = 0 & 1

-- | The unit vector in the negative X direction.
unit_X :: R2
unit_X = (-1) & 0

-- | The unit vector in the negative Y direction.
unit_Y :: R2
unit_Y = 0 & (-1)

-- | Compute the direction of a vector, measured counterclockwise from
--   the positive x-axis as a fraction of a full turn.  The zero
--   vector is arbitrarily assigned the direction 0.
direction :: Angle a => R2 -> a
direction (coords -> x :& y) = convertAngle . Rad $ atan2 y x

-- | Convert an angle into a unit vector pointing in that direction.
fromDirection :: Angle a => a -> R2
fromDirection a = cos a' & sin a'
  where Rad a' = convertAngle a

-- | A convenient synonym for 'fromDirection'.
e :: Angle a => a -> R2
e = fromDirection

-- | @perp v@ is perpendicular to and has the same magnitude as @v@.
--   In particular @perp v == rotateBy (1/4) v@.
perp :: R2 -> R2
perp (coords -> x :& y) = (-y) & x

-- | @leftTurn v1 v2@ tests whether the direction of @v2@ is a left
--   turn from @v1@ (that is, if the direction of @v2@ can be obtained
--   from that of @v1@ by adding an angle 0 <= theta <= tau/2).
leftTurn :: R2 -> R2 -> Bool
leftTurn v1 v2 = (v1 <.> perp v2) < 0
