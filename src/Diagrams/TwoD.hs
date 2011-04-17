{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
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
--   * "Diagrams.TwoD.Arc" defines circular arcs
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
         R2
       , P2
       , Angle
       , unitX, unitY

         -- * Shapes
       , circle
       , ellipse
       , arc

       , PolygonOrientation(..), PolygonOpts(..)
       , polygon, polygonPath, polygonVertices
       , square
       , starPolygon

         -- * Transformations
       , rotation, rotate
       , rotationBy, rotateBy
       , scalingX, scaleX
       , scalingY, scaleY
       , translationX, translateX
       , translationY, translateY
       , reflectionX, reflectX
       , reflectionY, reflectY

         -- * Combinators
       , (===), (|||)
       , hcat, hcat'
       , vcat, vcat'

         -- * Alignment
       , alignLeft, alignRight, alignTop, alignBottom
       , alignX, alignY
       , centerX, centerY, centerXY
       , strutX, strutY

         -- * Utilities
       , width, height, size2D
       , extentX, extentY, center2D
       ) where

import Diagrams.TwoD.Types
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Arc
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Align
import Diagrams.TwoD.Combinators
import Diagrams.TwoD.Util