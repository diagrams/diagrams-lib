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
--     diagrams, including types representing the 2D Euclidean vector
--     space and various systems of angle measurement.
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
--   * "Diagrams.TwoD.Path" exports various operations on
--     two-dimensional paths when viewed as regions of the plane.
--
--   * "Diagrams.TwoD.Shapes" defines other two-dimensional shapes,
--     e.g. various polygons.
--
--   * "Diagrams.TwoD.Util" defines some two-dimensional utilities,
--     such as unit vectors and functions for computing the size and
--     extent of diagrams in R^2.
--
--   * "Diagrams.TwoD.Model" defines some aids for visualizing
--     diagrams' internal model (local origins, bounding regions,
--     etc.)
--
-----------------------------------------------------------------------------
module Diagrams.TwoD
       ( -- * R^2
         R2
       , P2
       , unitX, unitY, unit_X, unit_Y, direction

         -- * Angles
       , tau
       , Angle(..)
       , CircleFrac(..), Rad(..), Deg(..)
       , fullCircle, convertAngle

         -- * Paths
         -- ** Stroking
       , stroke, strokeT

         -- ** Clipping
       , Clip(..), clipBy

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
       , rect
       , starPolygon
       , eqTriangle

         -- ** Other shapes
       , roundedRectPath, roundedRect

         -- * Transformations
         -- ** Rotation
       , rotation, rotate, rotateBy
       , rotationAbout, rotateAbout
         -- ** Scaling
       , scalingX, scaleX
       , scalingY, scaleY
       , scaling, scale
       , scaleToX, scaleToY
         -- ** Translation
       , translationX, translateX
       , translationY, translateY
       , translation, translate
         -- ** Reflection
       , reflectionX, reflectX
       , reflectionY, reflectY
       , reflectionAbout, reflectAbout

         -- * Combinators
       , strutX, strutY

       , (===), (|||)
       , hcat, hcat'
       , vcat, vcat'

         -- * Alignment
       , alignL, alignR, alignT, alignB, alignTL, alignTR, alignBL, alignBR
       , alignX, alignY
       , centerX, centerY, centerXY

         -- * Utilities
       , width, height, size2D
       , extentX, extentY, center2D

         -- * Visual aids for understanding the internal model
       , showOrigin
       ) where

import Diagrams.TwoD.Types
import Diagrams.TwoD.Path
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Arc
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Align
import Diagrams.TwoD.Combinators
import Diagrams.TwoD.Util
import Diagrams.TwoD.Model