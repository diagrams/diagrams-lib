{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module Diagrams.TwoD where

import "diagrams-core" Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Expressions
import Graphics.Rendering.Diagrams.Transform

import Data.VectorSpace
import Data.LinearMap

import qualified Data.Map as M
import Control.Arrow (first, second)

type P2 = (Double, Double)

{-
instance Transformable P2 where
  type TSpace P2 = P2
  transform = aapply
-}

instance Transformable P2 where
  type TSpace P2 = P2
  transform = papply

data Box = Box P2 P2 P2 P2

instance Transformable Box where
  type TSpace Box = P2
  transform a (Box v1 v2 v3 v4) = Box (transform a v1)
                                      (transform a v2)
                                      (transform a v3)
                                      (transform a v4)

box :: (BSpace b ~ P2, Renderable Box b) => Diagram b
box = Diagram [Prim (Box (-1,-1) (1,-1) (1,1) (-1,1))]
              (Bounds boxBounds)
              (M.fromList [ (nm "LL", (-1,-1))
                          , (nm "LR", ( 1,-1))
                          , (nm "UR", ( 1, 1))
                          , (nm "UL", (-1, 1)) ])
  where boxBounds (x,y) = let d = x*x + y*y  -- want u.v/u.u, where u=(x,y), v=(1,1),(1,-1),(-1,1),(-1,-1)
                          in  maximum [(-x-y)/d, (x-y)/d, (x+y)/d, (y-x)/d]

-- circle

type Radius = Double

data Ellipse = Ellipse Double Double Double Double Double Double
-- 6 Doubles are A, B, C, D, E, F in A x^2 + B x y + C y^2 + D x + E y + F = 0

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

type Angle = Double  -- in radians

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


