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

