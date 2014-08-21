{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Vector
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Three-dimensional vectors.
--
-----------------------------------------------------------------------------
module Diagrams.ThreeD.Vector
       ( -- * Special 2D vectors
         unitX, unitY, unitZ, unit_X, unit_Y, unit_Z, unit, unit_
       ) where

import           Diagrams.Coordinates
import           Diagrams.TwoD.Vector

import Linear.Vector hiding (unit)

-- | The unit vector in the positive Y direction.
unitZ :: (HasZ v, Additive v, Floating n) => v n
unitZ = unit _y

-- | The unit vector in the negative X direction.
unit_Z :: (HasZ v, Additive v, Floating n) => v n
unit_Z = unit_ _z
