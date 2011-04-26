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
--     diagrams.
--
--   * "Diagrams.TwoD.Align" defines alignment combinators specialized
--     to two dimensions (see "Diagrams.Align" for more general
--     alignment).
--
--   * "Diagrams.TwoD.Combinators" defines ways of combining diagrams
--     specialized to two dimensions (see also "Diagrams.Combinators"
--     for more general combining).
--
--   * "Diagrams.TwoD.Transform" defines R^2-specific transformations
--     such as rotation by an angle, and scaling, translation, and
--     reflection in the X and Y directions.
--
--   * "Diagrams.TwoD.Ellipse" defines ellipses.
--
--   * "Diagrams.TwoD.Arc" defines circular arcs.
--
--   * "Diagrams.TwoD.Shapes" defines other two-dimensional shapes,
--     e.g. various polygons.
--
--   * "Diagrams.TwoD.Util" defines some two-dimensional utilities,
--     such as unit vectors and functions for computing the size and
--     extent of diagrams in R^2.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD
       ( -- * R^2
         R2
       , P2
       , Angle
       , unitX, unitY

         -- * Shapes
         -- ** Rules
       , hrule, vrule

         -- ** Circle-ish things
       , circle
       , ellipse
       , arc

         -- ** General polygons
       , polygon, polygonPath, polygonVertices
       , PolygonOpts(..), PolygonOrientation(..)

         -- ** Special polygons
       , square
       , starPolygon
       , eqTriangle

         -- * Transformations
         -- ** Rotation
       , rotation, rotate
       , rotationBy, rotateBy
         -- ** Scaling
       , scalingX, scaleX
       , scalingY, scaleY
         -- ** Translation
       , translationX, translateX
       , translationY, translateY
         -- ** Reflection
       , reflectionX, reflectX
       , reflectionY, reflectY

         -- * Combinators
       , strutX, strutY

       , (===), (|||)
       , hcat, hcat'
       , vcat, vcat'

         -- * Alignment
       , alignLeft, alignRight, alignTop, alignBottom
       , alignX, alignY
       , centerX, centerY, centerXY

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