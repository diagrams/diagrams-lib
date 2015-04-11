{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines the three-dimensional vector space R^3,
-- three-dimensional transformations, and various predefined
-- three-dimensional shapes.  This module re-exports useful
-- functionality from a group of more specific modules:
--
--   * "Diagrams.ThreeD.Types" defines basic types for two-dimensional
--     diagrams, including types representing the 3D Euclidean vector
--     space and various systems of representing directions.
--
--   * "Diagrams.ThreeD.Transform" defines R^3-specific
--     transformations such as rotation by an angle, and scaling,
--     translation, and reflection in the X, Y, and Z directions.
--     "Diagrams.ThreeD.Deform" defines several R^3-specific
--     non-affine transformations, such as projections.
--
--   * "Diagrams.ThreeD.Shapes" defines three-dimensional solids,
--     e.g. spheres and cubes.
--
--   * "Diagrams.ThreeD.Vector" defines some special 3D vectors and
--     functions for converting between vectors and directions.
--
--   * "Diagrams.ThreeD.Light" and "Diagrams.ThreeD.Camera" define types needed
--     for rendering 3D geometry to (2D) images.
--
--   * "Diagrams.ThreeD.Align" defines many alignment combinators
--     specialized to three dimensions.
--
--   * "Diagrams.ThreeD.Attributes" defines 3D-specific attributes
--     such as surface color, diffuse reflectance, and specular
--     highlights.
-----------------------------------------------------------------------------
module Diagrams.ThreeD
       (
         module Diagrams.ThreeD.Align
       , module Diagrams.ThreeD.Attributes
       , module Diagrams.ThreeD.Camera
       , module Diagrams.ThreeD.Deform
       , module Diagrams.ThreeD.Light
       , module Diagrams.ThreeD.Shapes
       , module Diagrams.ThreeD.Transform
       , module Diagrams.ThreeD.Types
       , module Diagrams.ThreeD.Vector
       ) where

import           Diagrams.ThreeD.Align
import           Diagrams.ThreeD.Attributes
import           Diagrams.ThreeD.Camera
import           Diagrams.ThreeD.Deform
import           Diagrams.ThreeD.Light
import           Diagrams.ThreeD.Shapes
import           Diagrams.ThreeD.Transform
import           Diagrams.ThreeD.Types
import           Diagrams.ThreeD.Vector

