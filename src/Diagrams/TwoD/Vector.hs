{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
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
       ) where

import Control.Lens ((&), (.~))

import Data.VectorSpace
import Data.Basis

import Diagrams.Core.V
import Diagrams.Angle
import Diagrams.Direction
import Diagrams.TwoD.Types
import Diagrams.Coordinates

-- | The unit vector in the positive X direction.
unitX :: (LikeR2 v) => v
unitX = mkR2 1 0

-- | The unit vector in the positive Y direction.
unitY :: (HasBasis v, Num (Scalar v), Basis v ~ R2Basis) => v
unitY = mkR2 0 1

-- | The unit vector in the negative X direction.
unit_X :: (HasBasis v, Num (Scalar v), Basis v ~ R2Basis) => v
unit_X = mkR2 (-1) 0

-- | The unit vector in the negative Y direction.
unit_Y :: (HasBasis v, Num (Scalar v), Basis v ~ R2Basis) => v
unit_Y = mkR2 0 (-1)

-- | The origin of the direction AffineSpace.  For all d, @d .-. xDir
-- = d^._theta@.
xDir :: (HasBasis v, Num (Scalar v), Basis v ~ R2Basis) => Direction v
xDir = direction unitX

-- | A unit vector at a specified angle counterclockwise from the
-- positive X axis.
e :: (HasBasis v, Num (Scalar v), Basis v ~ R2Basis, HasTheta v) => Angle (Scalar (V v)) -> v
e a = unitX & _theta .~ a

-- | @perp v@ is perpendicular to and has the same magnitude as @v@.
--   In particular @perp v == rotateBy (1/4) v@.
perp :: (HasBasis v, Num (Scalar v), Basis v ~ R2Basis) => v -> v
perp (unr2 -> (x,y)) = mkR2 (-y) x

-- | @leftTurn v1 v2@ tests whether the direction of @v2@ is a left
--   turn from @v1@ (that is, if the direction of @v2@ can be obtained
--   from that of @v1@ by adding an angle 0 <= theta <= tau/2).
leftTurn :: (HasBasis v, Num (Scalar v), Ord (Scalar v), Basis v ~ R2Basis, InnerSpace v) => v -> v -> Bool
leftTurn v1 v2 = (v1 <.> perp v2) < 0
