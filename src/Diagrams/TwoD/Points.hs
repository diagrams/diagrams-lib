-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Points
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Special functions for points in R2.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Points where

import Data.List

import Diagrams.Core
import Diagrams.TwoD.Vector
import Diagrams.TwoD.Types (P2)

import Linear.Affine

-- | Find the convex hull of a list of points using Andrew's monotone chain
--   algorithm O(n log n).
--   
--   Returns clockwise list of points starting from the left-most point.
convexHull2D :: OrderedField n => [P2 n] -> [P2 n]
convexHull2D ps = init upper ++ reverse (tail lower)
  where
    (upper, lower) = sortedConvexHull (sort ps)

-- | Find the convex hull of a set of points already sorted in the x direction. 
--   The first list of the tuple is the upper hull going clockwise from 
--   left-most to right-most point. The second is the lower hull from 
--   right-most to left-most in the anti-clockwise direction.
sortedConvexHull :: OrderedField n => [P2 n] -> ([P2 n], [P2 n])
sortedConvexHull ps = (chain True ps, chain False ps)
 where
   chain upper (p1_:p2_:rest_) =
     case go (p2_ .-. p1_) p2_ rest_ of
       Right l -> p1_:l
       Left l  -> chain upper (p1_:l)
     where
       test = if upper then (>0) else (<0)
       -- find the convex hull by comparing the angles of the vectors with
       -- the cross product and backtracking if necessary
       go dir p1 l@(p2:rest)
         -- backtrack if the direction is outward
         | test $ dir `cross2` dir' = Left l
         | otherwise                =
             case go dir' p2 rest of
               Left m  -> go dir p1 m
               Right m -> Right (p1:m)
         where
           dir' = p2 .-. p1
       go _ p1 p = Right (p1:p)

   chain _ l = l
