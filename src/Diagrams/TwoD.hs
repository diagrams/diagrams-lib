{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Two
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- The two-dimensional vector space R^2, two-dimensional
-- transformations, and various predefined two-dimensional shapes.
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
import Graphics.Rendering.Diagrams.Expressions
import Graphics.Rendering.Diagrams.Transform

import Diagrams.TwoD.Types
import Diagrams.TwoD.Ellipse

import qualified Data.Map as M
import Control.Arrow (first, second)

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

-- rotation and scaling

-- Do we want to rotate things in arbitrary dimensions?

rotation :: Angle -> Projective P2
rotation theta = fromLinear $ rot theta <-> rot (-theta)
  where
    rot th (x,y) = (cos th * x - sin th * y, sin th * x + cos th * y)

rotate :: (TSpace t ~ P2, Transformable t) => Angle -> t -> t
rotate = transform . rotation

horizontalScale :: (TSpace t ~ P2, Transformable t) => Double -> t -> t
horizontalScale c = transform . fromLinear $ first (*c) <-> first (/c)

verticalScale :: (TSpace t ~ P2, Transformable t) => Double -> t -> t
verticalScale c = transform . fromLinear $ second (*c) <-> second (/c)


