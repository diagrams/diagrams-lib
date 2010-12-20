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
       , polygon
       , square
       , createStar
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
--   the generated polygon will be fit into the unit circle which has radius of 1. 
--   each point of the polygon will be located on the circumference of the unit circle
polygon:: (BSpace b ~ R2, Renderable (Path R2) b) => Int -> Diagram b
polygon n = stroke . polygonPath $ n

-- | Take numbers of sides as an input and generate the path to be rendered
--   a helper function for polygon
polygonPath:: Int -> Path R2
polygonPath n = close $ pathFromVertices (polygonVertices n)

-- | Construct list of vertices from the given sides 
--   a helper function for polygonPath
polygonVertices:: Int -> [P2]
polygonVertices n = map (vertexGen n) [1..(n)]
	where vertexGen:: Int -> Int -> P2
	      vertexGen n t  = let starting = translateX 1 origin
              	     		   angle    = (2*pi*fromIntegral t) / fromIntegral n
		    		   in rotate angle starting
   	      	 
-- | Generate a sqaure which has its center at the origin(0,0) 
--   each side of the square will have length of one unit
square ::  (BSpace b ~ R2, Renderable (Path R2) b) => Diagram b
square = stroke . close $ pathFromVertices $ [origin, translateX 1 origin, translateX 1 (translateY 1 origin), translateY 1 origin]


-- | Generate a star polygon 
--   take numbers of angle as one of its arguement
--   Formula: (pi*(p - 2q))/p ; p is the first arguement and q is the second arguement
createStar :: (BSpace b ~ R2, Renderable (Path R2) b) => Int -> Int-> Diagram b
createStar p q =  stroke . close $ pathFromVertices $ createStarH origin p q p 0

createStarH :: P2 -> Int -> Int -> Int -> Angle -> [P2]
createStarH _ _ _ 0 _ = []
createStarH v@(P r) p q n a = (point : createStarH point p q (n - 1) angle)
          where angle = (a + pi - (pi*(fromIntegral p-2*(fromIntegral q))/fromIntegral p))
                point = translateY (snd r) . translateX (fst r) $ rotate angle (translateY 1 origin)
