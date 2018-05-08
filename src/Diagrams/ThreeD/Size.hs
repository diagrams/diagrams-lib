{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Size
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Utilities for working with sizes of three-dimensional objects.
--
-----------------------------------------------------------------------------
module Diagrams.ThreeD.Size
       (
         -- ** Computing sizes
         extentX, extentY, extentZ

         -- ** Specifying sizes
       , mkSizeSpec3D
       , dims3D

       ) where

import           Diagrams.Core
import           Diagrams.Core.Envelope
import           Diagrams.Size
import           Diagrams.TwoD.Size
import           Diagrams.ThreeD.Types
import           Diagrams.ThreeD.Vector

------------------------------------------------------------
-- Computing diagram sizes
------------------------------------------------------------

-- | Compute the absolute z-coordinate range of an enveloped object in
--   the form @(lo,hi)@. Return @Nothing@ for objects with an empty
--   envelope.
extentZ :: (InSpace v n a, R3 v, Enveloped a) => a -> Maybe (n, n)
extentZ = extent unitZ

-- | Make a 'SizeSpec' from possibly-specified width and height.
mkSizeSpec3D :: Num n => Maybe n -> Maybe n -> Maybe n -> SizeSpec V3 n
mkSizeSpec3D x y z = mkSizeSpec (V3 x y z)

-- | Make a 'SizeSpec' from a width and height.
dims3D :: n -> n -> n -> SizeSpec V3 n
dims3D x y z = dims (V3 x y z)

