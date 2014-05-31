{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , ViewPatterns, ConstraintKinds
  #-}
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
         unitX, unitY, unitZ, unit_X, unit_Y, unit_Z,
       ) where

import Diagrams.Coordinates
import Diagrams.ThreeD.Types


-- | The unit vector in the positive X direction.
unitX :: (R3Ish v) => v
unitX = 1 ^& 0 ^& 0

-- | The unit vector in the positive Y direction.
unitY :: (R3Ish v) => v
unitY = 0 ^& 1 ^& 0

-- | The unit vector in the positive Z direction.
unitZ :: (R3Ish v) => v
unitZ = 0 ^& 0 ^& 1

-- | The unit vector in the negative X direction.
unit_X :: (R3Ish v) => v
unit_X = (-1) ^& 0 ^& 0

-- | The unit vector in the negative Y direction.
unit_Y :: (R3Ish v) => v
unit_Y = 0 ^& (-1) ^& 0

-- | The unit vector in the negative Z direction.
unit_Z :: (R3Ish v) => v
unit_Z = 0 ^& 0 ^& (-1)
