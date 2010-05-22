{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module Diagrams.TwoD where

import "diagrams-core" Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Expressions
import Graphics.Rendering.Diagrams.Transform

import Data.VectorSpace
import Data.LinearMap

import qualified Data.Map as M

type P2 = (Double, Double)

instance Transformable P2 where
  type TSpace P2 = P2
  transform = aapply

data Box = Box P2 P2 P2 P2

instance Transformable Box where
  type TSpace Box = P2
  transform a (Box v1 v2 v3 v4) = Box (transform a v1)
                                      (transform a v2)
                                      (transform a v3)
                                      (transform a v4)

box :: (BSpace b ~ P2, Renderable Box b) => Diagram b
box = Diagram [Prim (Box (-1,-1) (1,-1) (1,1) (-1,1))]
              boxBounds
              (M.fromList [ (nm "LL", (-1,-1))
                          , (nm "LR", ( 1,-1))
                          , (nm "UR", ( 1, 1))
                          , (nm "UL", (-1, 1)) ])
  where boxBounds (x,y) = let d = x*x + y*y  -- want u.v/u.u, where u=(x,y), v=(1,1),(1,-1),(-1,1),(-1,-1)
                          in  maximum [(-x-y)/d, (x-y)/d, (x+y)/d, (y-x)/d]

-- circle

type Radius = Double

data Circle = Circle P2 Radius

{-
need to handle ellipses

circle :: (BSpace b ~ P2, Renderable Circle b) => Diagram b
circle = Diagram [Prim (Circle (0,0) 1)]
                 circleBounds
                 (M.fromList [ (nm "C", ( 0, 0))
                             , (nm "E", ( 1, 0))
                             , (nm "N", ( 0, 1))
                             , (nm "W", (-1, 0))
                             , (nm "S", ( 0,-1)) ])
  where circleBounds (x,y) = 1 / sqrt(x*x + y*y)
-}

-- rotation



rotation theta = Affine (linear rot) zeroV where
    rot (x,y) = (costh * x - sinth * y,sinth * x + costh * y)
    costh = cos theta
    sinth = sin theta

rotate theta = transform $ rotation theta
