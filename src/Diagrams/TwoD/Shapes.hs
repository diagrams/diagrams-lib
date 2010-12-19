{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Shapes
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Various two-dimensional shapes.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Shapes
       ( Box(..)
       , box
       , polygonDiagrams
       ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types

import Diagrams.Path

import Diagrams.TwoD.Transform

import qualified Data.Map as M

import Data.Monoid (Any(..))

data Box = Box P2 P2 P2 P2
           deriving (Show)

instance Transformable Box where
  type TSpace Box = R2
  transform a (Box p1 p2 p3 p4) = Box (transform a p1)
                                      (transform a p2)
                                      (transform a p3)
                                      (transform a p4)

box :: (BSpace b ~ R2, Renderable Box b) => Diagram b
box = Diagram (prim $ Box (P (-1,-1)) (P (1,-1)) (P (1,1)) (P (-1,1)))
              (Bounds boxBounds)
              (fromNames [ ("LL", P (-1,-1))
                         , ("LR", P ( 1,-1))
                         , ("UR", P ( 1, 1))
                         , ("UL", P (-1, 1)) ])
              (\(P (x,y)) -> Any (inBox x && inBox y))
  where boxBounds (x,y) = let d = x*x + y*y  -- want u.v/u.u, where u=(x,y), v=(1,1),(1,-1),(-1,1),(-1,-1)
                              in  maximum [(-x-y)/d, (x-y)/d, (x+y)/d, (y-x)/d]
        inBox x = x >= (-1) && x <= 1

-- | Take number of sides of polygon then create diagrams with these numbers of equal length  sides
polygonDiagrams:: (BSpace b ~ R2, Renderable (Path R2) b) => Int -> Diagram b
polygonDiagrams n = stroke (polygonPath (polygonVertices n))

-- | Build a path from the given list of vertices
polygonPath:: [P2] -> Path R2
polygonPath xs = close $ pathFromVertices xs

-- | Construct list of vertices from the given sides 
polygonVertices:: Int -> [P2]
polygonVertices n = map (aux n) [1..(n)]

-- | helper function
--   first arguement = numbers of side
--   second arguement = times
aux:: Int -> Int -> P2
aux n t = rotate angle starting
   where starting = translateX 1 origin
         angle = (2*pi*fromIntegral t) / fromIntegral n

{--
-- | Equilateral triangle path Builder
--   a function to construct the path for drawing equilateral triangle
eqTrianglePath :: P2 -> Path R2
eqTrianglePath p = pathFromOffsets p (initPointTri p)

-- | Equilateral triangle drawing function
eqtriangleDiagram :: (BSpace b ~ R2, Renderable (Path R2) b) =>  P2 -> Diagram b
eqTriangleDiagram = stroke $ eqTrianglePath

-- | Equilateral offset genrator
initPointEqTri :: P2 -> [R2]
initPointEqTri (P r) = r1 : r2 : []
    where r1 = translateY (snd r) (translateX (fst r) (rotate (pi/3) r))
          r2 = translateY (snd r1) (translateX (fst r1) (rotate (pi/3) r1))

-- | BuildTriangleDiagram, create a triangle of specific angle
buildingTri :: Angle -> Angle -> P2 -> Diagram R2
buildingTri = undefined

-- | BuildTrianglePath, create a path for drawing a diagram
buildingTriPath :: Angle -> Angle


-- | Box Diagram
createBox:: (BSpace b ~ R2, Renderable (Path R2) b) =>  P2 -> Diagram b 
createBox p = stroke.close.pathFromVertices $ [p, translateX 1 p, translateX 1 . translateY 1 p, translateY 1 p]
--}

{--NOT WORK
-- | Polygon drawing function, draw a polygon of maximum side of 6
--   create a polygon of side (n) up to 6. The default length of each side is 1
polygonDrawing :: Int -> P2 -> Diagram R2
polygonDrawing

-- | Polygon path building function, create a path for the polygonDrawing
--   create a path for drawing different types of polygon beginning at the given starting point
--   Nothing if side > 6 or side < 1; otherwise Maybe
buildPolyPath :: Int -> P2 -> Maybe (Path R2)
buildPolyPath s p 
	| s == 1 = undefined
	| s == 2 = undefined 
	| s == 3 = undefined
	| s == 4 = undefined 
	| s == 5 = undefined
	| s == 6 = undefined 
	| otherwise = Nothing

-- | BuildDiagram, create a diagram from the given path
--   if Nothing then draw none, otherwise create a diagram
buildDiagram :: Maybe (Path R2) -> Diagram R2
buildDiagram Nothing   = undefined
buildDiagram Just path = stroke path--}



