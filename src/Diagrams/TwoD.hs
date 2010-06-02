{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the two-dimensional vector space R^2,
-- two-dimensional transformations, and various predefined
-- two-dimensional shapes.  This module re-exports useful
-- functionality from a group of more specific modules:
--
--   * "Diagrams.TwoD.Types" defines basic types for two-dimensional
--     diagrams
--
--   * "Diagrams.TwoD.Transform" defines various 2D-specific
--     transformations
--
--   * "Diagrams.TwoD.Ellipse" defines ellipses
--
--   * "Diagrams.TwoD.Shapes" defines various other two-dimensional
--     shapes
--
-- For most uses it should be sufficient to simply import
-- "Diagrams.TwoD"; occasionally users may wish to import one or more
-- of the above modules directly to access more specialized/internal
-- functionality.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD
       ( -- * R^2
         P2
       , Angle

         -- * Transformations
       , rotation, rotate
       , horizontalScale
       , verticalScale

         -- * Shapes
       , box
       , circle
       , ellipse
       ) where

import "diagrams-core" Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Shapes
