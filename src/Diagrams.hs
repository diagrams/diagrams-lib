{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams
-- Copyright   :  (c) 2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module only contains exports defined in "diagrams-lib" or
-- "diagrams-core". This module is less likely conflict with any
-- other modules but importing "Diagrams.Prelude" is often more convenient.
--
-----------------------------------------------------------------------------

module Diagrams
  (
    -- * Core library
    -- | The core definitions of transformations, diagrams,
    --   backends, and so on.
    module Diagrams.Core

    -- * Standard library

    -- | Attributes (color, line style, etc.) and styles.
  , module Diagrams.Attributes

    -- | Alignment of diagrams relative to their envelopes.
  , module Diagrams.Align

    -- | Creating and using bounding boxes.
  , module Diagrams.BoundingBox

    -- | Combining multiple diagrams into one.
  , module Diagrams.Combinators

    -- | Giving concrete locations to translation-invariant things.
  , module Diagrams.Located

    -- | Linear and cubic bezier segments.
  , module Diagrams.Segment

    -- | Trails.
  , module Diagrams.Trail

    -- | Parametrization of segments and trails.
  , module Diagrams.Parametric

    -- | Adjusting the length of parameterized objects.
  , module Diagrams.Parametric.Adjust

    -- | Computing tangent and normal vectors of segments and
    --   trails.
  , module Diagrams.Tangent

    -- | Trail-like things.
  , module Diagrams.TrailLike

    -- | Paths.
  , module Diagrams.Path

    -- | Cubic splines.
  , module Diagrams.CubicSpline

    -- | Some additional transformation-related functions, like
    --   conjugation of transformations.
  , module Diagrams.Transform

    -- | Projective transformations and other deformations
    -- lacking an inverse.
  , module Diagrams.Deform

    -- | Giving names to subdiagrams and later retrieving
    --   subdiagrams by name.
  , module Diagrams.Names

    -- | Envelopes, aka functional bounding regions.
  , module Diagrams.Envelope

    -- | Traces, aka embedded raytracers, for finding points on
    --   the boundary of a diagram.
  , module Diagrams.Trace

    -- | A query is a function that maps points in a vector space
    --   to values in some monoid; they can be used to annotate
    --   the points of a diagram with some values.
  , module Diagrams.Query

    -- | Utilities for working with points.
  , module Diagrams.Points

    -- | Utilities for working with size.
  , module Diagrams.Size

    -- | Angles
  , module Diagrams.Angle

    -- | Convenience infix operators for working with coordinates.
  , module Diagrams.Coordinates

    -- | Directions, distinguished from angles or vectors
  , module Diagrams.Direction

    -- | A wide range of things (shapes, transformations,
    --   combinators) specific to creating two-dimensional
    --   diagrams.
  , module Diagrams.TwoD

    -- | Extra things for three-dimensional diagrams.
  , module Diagrams.ThreeD

    -- | Tools for making animations.
  , module Diagrams.Animation

    -- | Various utility definitions.
  , module Diagrams.Util

  ) where

import           Diagrams.Core

import           Diagrams.Align
import           Diagrams.Angle
import           Diagrams.Animation
import           Diagrams.Attributes
import           Diagrams.BoundingBox       hiding (intersection, union, inside, outside, contains)
import           Diagrams.Combinators
import           Diagrams.Coordinates
import           Diagrams.CubicSpline
import           Diagrams.Deform
import           Diagrams.Direction         hiding (dir)
import           Diagrams.Envelope
import           Diagrams.Located
import           Diagrams.Names
import           Diagrams.Parametric
import           Diagrams.Parametric.Adjust
import           Diagrams.Path
import           Diagrams.Points
import           Diagrams.Query
import           Diagrams.Segment
import           Diagrams.Size
import           Diagrams.Tangent
import           Diagrams.ThreeD
import           Diagrams.Trace
import           Diagrams.Trail             hiding (linePoints, loopPoints,
                                             trailPoints)
import           Diagrams.TrailLike
import           Diagrams.Transform
import           Diagrams.TwoD
import           Diagrams.Util

