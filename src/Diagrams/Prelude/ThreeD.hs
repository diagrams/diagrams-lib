{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Prelude.ThreeD
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A module to re-export most of the functionality of the diagrams
-- core and standard library, including 3D types and functions.
--
-----------------------------------------------------------------------------

module Diagrams.Prelude.ThreeD
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

         -- | Angles
       , module Diagrams.Angle

         -- | Convenience infix operators for working with coordinates.
       , module Diagrams.Coordinates

       -- | Directions, distinguished from angles or vectors
       , module Diagrams.Direction

         -- | things (shapes, transformations, combinators) specific
         -- to creating three-dimensional diagrams.
       , module Diagrams.ThreeD

         -- | Tools for making animations.
       , module Diagrams.Animation

         -- | Various utility definitions.
       , module Diagrams.Util

         -- * Convenience re-exports
         -- | For representing and operating on colors.
       , module Data.Colour
         -- | A large list of color names.
       , module Data.Colour.Names
         -- | Semigroups and monoids show up all over the place, so things from
         --   Data.Semigroup and Data.Monoid often come in handy.
       , module Data.Semigroup
         -- | For computing with vectors.
       , module Linear.Vector
         -- | For computing with points and vectors.
       , module Linear.Affine

         -- | For working with 'Active' (i.e. animated) things.
       , module Data.Active

         -- | Essential Lens Combinators
       , (&), (.~), (%~)

       , Applicative(..), (*>), (<*), (<$>), (<$), liftA, liftA2, liftA3
       ) where

import           Diagrams.Core

import           Diagrams.Align
import           Diagrams.Angle
import           Diagrams.Animation
import           Diagrams.Attributes
import           Diagrams.Combinators
import           Diagrams.Coordinates
import           Diagrams.CubicSpline
import           Diagrams.Deform
import           Diagrams.Direction
import           Diagrams.Envelope
import           Diagrams.Located
import           Diagrams.Names
import           Diagrams.Parametric
import           Diagrams.Parametric.Adjust
import           Diagrams.Path
import           Diagrams.Points
import           Diagrams.Query
import           Diagrams.Segment
import           Diagrams.Tangent
import           Diagrams.ThreeD
import           Diagrams.Trace
import           Diagrams.Trail hiding (trailPoints, loopPoints, linePoints)
import           Diagrams.TrailLike
import           Diagrams.Transform
import           Diagrams.Util

import           Control.Applicative
import           Control.Lens               ((%~), (&), (.~))
import           Data.Active
import           Data.Colour                hiding (AffineSpace (..), atop, over)
import           Data.Colour.Names          hiding (tan)
import           Data.Semigroup

import Linear.Affine
import Linear.Vector hiding (unit)
