{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
--   * "Diagrams.TwoD.Ellipse" defines circles and ellipses.
--
--   * "Diagrams.TwoD.Arc" defines circular arcs.
--
--   * "Diagrams.TwoD.Path" exports various operations on
--     two-dimensional paths when viewed as regions of the plane.
--
--   * "Diagrams.TwoD.Polygons" defines general algorithms for drawing
--     various types of polygons.
--
--   * "Diagrams.TwoD.Shapes" defines other two-dimensional shapes,
--     e.g. various polygons.
--
--   * "Diagrams.TwoD.Arrow" contains tools for drawing arrows between
--     things.
--
--   * "Diagrams.TwoD.Text" defines primitive text diagrams.
--
--   * "Diagrams.TwoD.Image" allows importing external images into diagrams.
--
--   * "Diagrams.TwoD.Vector" defines some special 2D vectors and
--     functions for converting between vectors and angles.
--
--   * "Diagrams.TwoD.Size" defines functions for working with the
--     size of 2D objects.
--
--   * "Diagrams.TwoD.Model" defines some aids for visualizing
--     diagrams' internal model (local origins, envelopes, etc.)
--
-----------------------------------------------------------------------------
module Diagrams.TwoD
       ( -- * R^2
         R2, r2, unr2, mkR2
       , P2, p2, unp2, mkP2
       , T2
       , unitX, unitY, unit_X, unit_Y
       , direction, fromDirection

         -- * Angles
       , tau

         -- * Paths
         -- ** Stroking
       , stroke, stroke', strokeTrail, strokeT, strokeTrail', strokeT'
       , strokeLine, strokeLoop
       , strokeLocTrail, strokeLocT, strokeLocLine, strokeLocLoop
       , FillRule(..), fillRule
       , StrokeOpts(..), vertexNames, queryFillRule
       , splitFills

         -- ** Clipping
       , clipBy, clipTo, clipped

         -- * Shapes
         -- ** Rules
       , hrule, vrule

         -- ** Circle-ish things
       , unitCircle
       , circle
       , ellipse
       , ellipseXY
       , arc
       , arc'
       , arcCW
       , wedge
       , arcBetween
       , annularWedge

         -- ** General polygons
       , polygon, polyTrail
       , PolygonOpts(..), polyType, polyOrient, polyCenter
       , PolyType(..), PolyOrientation(..)

         -- ** Star polygons
       , StarOpts(..), star

         -- ** Regular polygons
       , regPoly
       , triangle
       , eqTriangle
       , square
       , pentagon
       , hexagon
       , heptagon
       , septagon
       , octagon
       , nonagon
       , decagon
       , hendecagon
       , dodecagon

         -- ** Other special polygons
       , unitSquare
       , rect

         -- ** Other shapes
       , roundedRect, roundedRect'
       , RoundedRectOpts(..), radiusTL, radiusTR, radiusBL, radiusBR

         -- ** Arrows
       , arrowV, arrowV'
       , arrowAt, arrowAt'
       , arrowBetween, arrowBetween'
       , connect, connect'
       , connectPerim, connectPerim'
       , connectOutside, connectOutside'
       , arrow, arrow'
       , straightShaft
       , module Diagrams.TwoD.Arrowheads

       , ArrowOpts(..)

       , arrowHead
       , arrowTail
       , arrowShaft
       , headGap
       , tailGap
       , gaps, gap
       , headColor
       , headStyle
       , tailColor
       , tailStyle
       , shaftColor
       , shaftStyle
       , headSize
       , tailSize

         -- * Text
       , text, topLeftText, alignedText, baselineText
       , font, italic, oblique, bold, fontSize
       , fontSizeO, fontSizeL, fontSizeN, fontSizeG

         -- * Images
       , DImage(..), ImageData(..)
       , Embedded, External
       , image
       , loadImageEmb
       , loadImageExt
       , uncheckedImageRef
       , raster
       , rasterDia

         -- * Transformations
         -- ** Rotation
       , rotation, rotate, rotateBy
       , rotationAbout, rotateAbout
         -- ** Scaling
       , scalingX, scaleX
       , scalingY, scaleY
       , scaling, scale
       , scaleToX, scaleToY
       , scaleUToX, scaleUToY
         -- ** Translation
       , translationX, translateX
       , translationY, translateY
       , translation, translate
         -- ** Reflection
       , reflectionX, reflectX
       , reflectionY, reflectY
       , reflectionAbout, reflectAbout
         -- ** Shears
       , shearingX, shearX
       , shearingY, shearY

         -- * Deformations - non-affine transforms
       , parallelX0, perspectiveX1, parallelY0, perspectiveY1
       , facingX, facingY

         -- * Combinators
         -- ** Combining multiple diagrams
       , (===), (|||), atAngle
       , hcat, hcat'
       , vcat, vcat'

         -- ** Spacing and envelopes
       , strutX, strutY
       , padX, padY

       , extrudeLeft, extrudeRight, extrudeBottom, extrudeTop

       , view

         -- ** Background

       , boundingRect, bg

         -- * Alignment
       , alignL, alignR, alignT, alignB, alignTL, alignTR, alignBL, alignBR
       , alignX, alignY
       , centerX, centerY, centerXY

         -- * Snugging
       , snugL, snugR, snugT, snugB, snugTL, snugTR, snugBL, snugBR
       , snugX, snugY
       , snugCenterX, snugCenterY, snugCenterXY

         -- * Size
         -- ** Computing size
       , width, height, size2D, sizeSpec2D
       , extentX, extentY, center2D

         -- ** Specifying size
       , SizeSpec2D(..)
       , mkSizeSpec

         -- ** Adjusting size
       , sized, sizedAs

        -- * Textures
      , Texture(..), SpreadMethod(..), GradientStop(..), mkStops, getFillTexture
      , fillTexture, getLineTexture, lineTexture, lineTextureA
      , stopFraction, stopColor

      , LGradient(..), lGradStops, lGradTrans, lGradStart, lGradEnd
      , lGradSpreadMethod, defaultLG, _LG, mkLinearGradient

      , RGradient(..)
      , rGradStops, rGradCenter0, rGradRadius0, rGradCenter1, rGradRadius1
      , rGradTrans, rGradSpreadMethod, defaultRG, _RG, mkRadialGradient

        -- ** Colors
      , fillColor, fc, fcA, recommendFillColor, getFillColor
      , lineColor, lc, lcA, lineColorA, getLineColor

       -- ** Width
       , LineWidth, getLineWidth, lineWidth, lineWidthA
       , lw, lwN, lwO, lwL, lwG
       , ultraThin, veryThin, thin, medium, thick, veryThick, ultraThick, none
       , tiny, verySmall, small, normal, large, veryLarge, huge

       -- ** Dashing
       , Dashing(..), DashingA, getDashing
       , dashing, dashingO, dashingL, dashingN, dashingG

         -- * Visual aids for understanding the internal model
       , showOrigin
       , showOrigin'
       , OriginOpts(..), oColor, oScale, oMinSize
       , showLabels

       ) where

import           Diagrams.TwoD.Align
import           Diagrams.TwoD.Arc
import           Diagrams.TwoD.Arrow
import           Diagrams.TwoD.Arrowheads
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Combinators
import           Diagrams.TwoD.Deform
import           Diagrams.TwoD.Ellipse
import           Diagrams.TwoD.Image
import           Diagrams.TwoD.Model
import           Diagrams.TwoD.Path
import           Diagrams.TwoD.Polygons
import           Diagrams.TwoD.Shapes
import           Diagrams.TwoD.Size
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector

import           Diagrams.Util             (tau)
