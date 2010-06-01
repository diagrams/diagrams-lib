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
       ) where

import "diagrams-core" Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Shapes

{-
v v v v v v v
*************
import qualified Data.Map as M

type P2 = (Double, Double)
type P3 = (Double, Double, Double)
type Aug3 = (P2,Double)

type Angle = Double  -- in radians

type FlatFunc = P3 -> P3
type AugFunc = Aug3 -> Aug3

instance Transformable P2 where
  type TSpace P2 = P2
  transform = papply

data Box = Box P2 P2 P2 P2
           deriving (Show)

instance Transformable Box where
  type TSpace Box = P2
  transform a (Box v1 v2 v3 v4) = Box (transform a v1)
                                      (transform a v2)
                                      (transform a v3)
                                      (transform a v4)

box :: (BSpace b ~ P2, Renderable Box b) => Diagram b
box = Diagram [Prim (Box (-1,-1) (1,-1) (1,1) (-1,1))]
              (Bounds boxBounds)
              (fromNames [ ("LL", (-1,-1))
                         , ("LR", ( 1,-1))
                         , ("UR", ( 1, 1))
                         , ("UL", (-1, 1)) ])
  where boxBounds (x,y) = let d = x*x + y*y  -- want u.v/u.u, where u=(x,y), v=(1,1),(1,-1),(-1,1),(-1,-1)
                          in  maximum [(-x-y)/d, (x-y)/d, (x+y)/d, (y-x)/d]
^ ^ ^ ^ ^ ^ ^
-}
