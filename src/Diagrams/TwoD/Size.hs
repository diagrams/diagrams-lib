{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Size
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Utilities for working with sizes of two-dimensional objects.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Size
       (
         -- ** Computing sizes
         width, height
       , extentX, extentY

         -- ** Specifying sizes
       , mkSizeSpec2D
       , mkWidth
       , mkHeight
       , dims2D

       ) where

import           Diagrams.Core
import           Diagrams.Core.Envelope
import           Diagrams.Size
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector

------------------------------------------------------------
-- Computing diagram sizes
------------------------------------------------------------

-- | Compute the width of an enveloped object.
--
--   Note this is just @diameter unitX@.
width :: (InSpace V2 n a, Enveloped a) => a -> n
width = diameter unitX

-- | Compute the height of an enveloped object.
height :: (InSpace V2 n a, Enveloped a) => a -> n
height = diameter unitY

-- | Compute the absolute x-coordinate range of an enveloped object in
--   the form @(lo,hi)@. Return @Nothing@ for objects with an empty
--   envelope.
--
--   Note this is just @extent unitX@.
extentX :: (InSpace v n a, R1 v, Enveloped a) => a -> Maybe (n, n)
extentX = extent unitX

-- | Compute the absolute y-coordinate range of an enveloped object in
--   the form @(lo,hi)@. Return @Nothing@ for objects with an empty
--   envelope.
extentY :: (InSpace v n a, R2 v, Enveloped a) => a -> Maybe (n, n)
extentY = extent unitY

-- | Make a 'SizeSpec' from possibly-specified width and height.
mkSizeSpec2D :: Num n => Maybe n -> Maybe n -> SizeSpec V2 n
mkSizeSpec2D x y = mkSizeSpec (V2 x y)

-- | Make a 'SizeSpec' from a width and height.
dims2D :: n -> n -> SizeSpec V2 n
dims2D x y = dims (V2 x y)

-- | Make a 'SizeSpec' with only width defined.
mkWidth :: Num n => n -> SizeSpec V2 n
mkWidth w = dims (V2 w 0)

-- | Make a 'SizeSpec' with only height defined.
mkHeight :: Num n => n -> SizeSpec V2 n
mkHeight h = dims (V2 0 h)

