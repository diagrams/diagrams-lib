{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
module Diagrams.TwoD where

import "diagrams-core" Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Backends
import Graphics.Rendering.Diagrams.Expressions
import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Renderable

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
  where boxBounds (x,y) = let d = x*x + y*y
                          in  maximum [(-x-y)/d, (x-y)/d, (x+y)/d, (y-x)/d]
