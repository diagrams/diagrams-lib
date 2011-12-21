{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Prelude
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A module to re-export most of the functionality of the diagrams
-- core and standard library.
--
-----------------------------------------------------------------------------

module Diagrams.Prelude
       (
         -- * Core library
         -- | The core definitions of transformations, diagrams,
         --   backends, and so on.
         module Graphics.Rendering.Diagrams

         -- * Standard library
         -- | Attributes (color, line style, etc.) and styles.
       , module Diagrams.Attributes

         -- | General alignment of diagrams.
       , module Diagrams.Align

         -- | Combining multiple diagrams into one.
       , module Diagrams.Combinators

         -- | Linear and cubic bezier segments.
       , module Diagrams.Segment

         -- | Trails and paths.
       , module Diagrams.Path

         -- | Cubic splines.
       , module Diagrams.CubicSpline

         -- | Some additional transformation-related functions, like
         --   conjugation of transformations.
       , module Diagrams.Transform

         -- | Convenient definitions and utilities for working with
         --   good old-fashioned, axis-aligned bounding boxes.
       , module Diagrams.BoundingBox

         -- | A wide range of things (shapes, transformations,
         --   combinators) specific to creating two-dimensional
         --   diagrams.
       , module Diagrams.TwoD

         -- | Various utility definitions.
       , module Diagrams.Util

         -- * Convenience re-exports
         -- | A large list of color names.
       , module Data.Colour.Names
         -- | Semigroups and monoids show up all over the place, so things from
         --   Data.Semigroup and Data.Monoid often come in handy.
       , module Data.Semigroup
         -- | For computing with vectors.
       , module Data.VectorSpace
         -- | For computing with points and vectors.
       , module Data.AffineSpace

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Attributes
import Diagrams.Align
import Diagrams.Combinators
import Diagrams.Segment
import Diagrams.Path
import Diagrams.CubicSpline
import Diagrams.Transform
import Diagrams.BoundingBox
import Diagrams.TwoD
import Diagrams.Util

import Data.Colour.Names
import Data.Semigroup
import Data.VectorSpace hiding (Sum(..))
import Data.AffineSpace
