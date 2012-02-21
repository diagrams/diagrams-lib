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

import Diagrams.TwoD.Types

-- | The unit vector in the positive X direction.
unitX :: R2
unitX = r2 (1,0)

-- | The unit vector in the positive Y direction.
unitY :: R2
unitY = r2 (0,1)

-- | The unit vector in the negative X direction.
unit_X :: R2
unit_X = r2 (-1,0)

-- | The unit vector in the negative Y direction.
unit_Y :: R2
unit_Y = r2 (0,-1)

-- | Compute the direction of a vector, measured counterclockwise from
--   the positive x-axis as a fraction of a full turn.  The zero
--   vector is arbitrarily assigned the direction 0.
direction :: Angle a => R2 -> a
direction (unr2 -> (x,y)) = convertAngle . Rad $ atan2 y x

-- | Convert an angle into a unit vector pointing in that direction.
fromDirection :: Angle a => a -> R2
fromDirection a = r2 (cos a', sin a')
  where Rad a' = convertAngle a

-- | A convenient synonym for 'fromDirection'.
e :: Angle a => a -> R2
e = fromDirection