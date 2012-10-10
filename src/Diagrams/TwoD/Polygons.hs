{-# LANGUAGE TypeFamilies
           , ScopedTypeVariables
           , DeriveFunctor
           , ExistentialQuantification
           , ViewPatterns
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Polygons
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines a general API for creating various types of
-- polygons.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Polygons(
        -- * Polygons
          PolyType(..)
        , PolyOrientation(..)
        , PolygonOpts(..)
        , polyVertices

        , polygon

        -- ** Generating polygon vertices

        , polyPolarVs
        , polySidesVs, polySidesVs'
        , polyRegularVs

        , orient

        -- * Star polygons
        , StarOpts(..)
        , star

        -- ** Function graphs
        -- $graphs
        , GraphPart(..)
        , orbits, mkGraph

    ) where

import Data.Ord          (comparing)
import Data.List         (maximumBy)
import Data.Maybe        (catMaybes)
import Data.Monoid       (mconcat)

import Control.Monad     (forM, liftM)

import Control.Monad.ST  (runST, ST)
import Data.Array.ST     (STUArray, newArray, readArray, writeArray)

import Data.AffineSpace  ((.-.), (.+^))
import Data.VectorSpace  (magnitude, normalized, project, (<.>), (^*))
import Data.Default

import Diagrams.Core

import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Vector (unitX, unitY, unit_Y)
import Diagrams.Path
import Diagrams.Points      (centroid)
import Diagrams.Util        ((#), tau)

-- | Method used to determine the vertices of a polygon.
data PolyType = forall a. Angle a => PolyPolar [a] [Double]
                -- ^ A \"polar\" polygon.
                --
                --   * The first argument is a list of /central/
                --     /angles/ from each vertex to the next.
                --
                --   * The second argument is a list of /radii/ from
                --     the origin to each successive vertex.
                --
                --   To construct an /n/-gon, use a list of /n-1/
                --   angles and /n/ radii.  Extra angles or radii
                --   are ignored.
                --
                --   Cyclic polygons (with all vertices lying on a
                --   circle) can be constructed using a second
                --   argument of @(repeat r)@.

              | forall a. Angle a => PolySides [a] [Double]
                -- ^ A polygon determined by the distance between
                --   successive vertices and the angles formed by
                --   each three successive vertices.  In other
                --   words, a polygon specified by \"turtle
                --   graphics\": go straight ahead x1 units; turn by
                --   angle a1; go straght ahead x2 units; turn by
                --   angle a2; etc. The polygon will be centered at
                --   the /centroid/ of its vertices.
                --
                --   * The first argument is a list of /vertex/
                --     /angles/, giving the angle at each vertex
                --     from the previous vertex to the next.  The
                --     first angle in the list is the angle at the
                --     /second/ vertex; the first edge always starts
                --     out heading in the positive y direction from
                --     the first vertex.
                --
                --   * The second argument is a list of distances
                --     between successive vertices.
                --
                --   To construct an /n/-gon, use a list of /n-2/
                --   angles and /n-1/ edge lengths.  Extra angles or
                --   lengths are ignored.

              | PolyRegular Int Double
                -- ^ A regular polygon with the given number of
                --   sides (first argument) and the given radius
                --   (second argument).

-- | Determine how a polygon should be oriented.
data PolyOrientation = NoOrient     -- ^ No special orientation; the first
                                    --   vertex will be at (1,0).
                                    --   This is the default.
                     | OrientH      -- ^ Orient /horizontally/, so the
                                    --   bottommost edge is parallel to
                                    --   the x-axis.
                     | OrientV      -- ^ Orient /vertically/, so the
                                    --   leftmost edge is parallel to the
                                    --   y-axis.
                     | OrientTo R2  -- ^ Orient so some edge is
                                    --   /facing/ /in/ /the/ /direction/
                                    --   /of/, that is, perpendicular
                                    --   to, the given vector.
  deriving (Eq, Ord, Show, Read)

-- | Options for specifying a polygon.
data PolygonOpts = PolygonOpts
                   { polyType       :: PolyType
                     -- ^ Specification for the polygon's vertices.

                   , polyOrient     :: PolyOrientation
                     -- ^ Should a rotation be applied to the
                     --   polygon in order to orient it in a
                     --   particular way?

                   , polyCenter     :: P2
                     -- ^ Should a translation be applied to the
                     --   polygon in order to place the center at a
                     --   particular location?
                   }

-- | The default polygon is a regular pentagon of radius 1, centered
--   at the origin, aligned to the x-axis.
instance Default PolygonOpts where
    def = PolygonOpts (PolyRegular 5 1) OrientH origin

-- | Generate the vertices of a polygon.  See 'PolygonOpts' for more
--   information.
polyVertices :: PolygonOpts -> [P2]
polyVertices po = moveTo (polyCenter po) ori
    where
        ps = case polyType po of
            PolyPolar ans szs -> polyPolarVs ans szs
            PolySides ans szs -> polySidesVs ans szs
            PolyRegular n r   -> polyRegularVs n r
        ori = case polyOrient po of
            OrientH      -> orient unit_Y ps
            OrientV      -> orient unitX  ps
            OrientTo v   -> orient v      ps
            NoOrient     -> ps

polygon :: (PathLike p, V p ~ R2) => PolygonOpts -> p
polygon opts = case pts of
                 []     -> pathLike origin True []
                 (p1:_) -> pathLike p1 True (segmentsFromVertices pts)
  where pts = polyVertices opts

-- | Generate the vertices of a polygon specified by polar data
--   (central angles and radii). See 'PolyPolar'.
polyPolarVs :: Angle a => [a] -> [Double] -> [P2]
polyPolarVs ans ls = zipWith (\a l -> rotate a . scale l $ p2 (1,0))
                             (scanl (+) 0 ans)
                             ls

-- | Generate the vertices of a polygon specified by side length and
--   angles, with the origin corresponding to the first vertex.  See
--   'PolySides'.
polySidesVs' :: Angle a => [a] -> [Double] -> [P2]
polySidesVs' ans ls = scanl (.+^) origin
                      $ zipWith rotate ans' (map (unitY ^*) ls)
  where
    ans' = scanl (+) 0 ans

-- | Generate the vertices of a polygon specified by side length and
--   angles, with the origin placed at the centroid.  See 'PolySides'.
polySidesVs :: Angle a => [a] -> [Double] -> [P2]
polySidesVs ans ls = p0 # moveOriginTo (centroid p0)
  where p0 = polySidesVs' ans ls

-- | Generate the vertices of a regular polygon.  See 'PolyRegular'.
polyRegularVs :: Int -> Double -> [P2]
polyRegularVs n r = polyPolarVs (take (n-1) . repeat $ (tau::Rad) / fromIntegral n)
                                (repeat r)

-- | Orient a list of points, rotating them as little as possible.
--   The points are rotated so that the edge furthest in the direction
--   of the given vector is perpendicular to it.  (Note: this may do odd
--   things to non-convex lists of points.)
orient :: R2 -> [P2] -> [P2]
orient _ [] = []
orient v xs = rotate a xs
    where
        (n1,x,n2) = maximumBy (comparing (distAlong v . sndOf3))
                       (zip3 (tail xs ++ take 1 xs) xs (last xs : init xs))
        distAlong w ((.-. origin) -> p) = signum (w <.> p) * magnitude (project w p)
        x'        = maximumBy (comparing (distAlong v)) [n1, n2]
        e         = x' .-. x
        th        = Rad $ acos ((e <.> normalized v) / magnitude e)
        a | rightTurn (x .+^ v) x x' = tau/4 - th
          | otherwise                = th - tau/4
        sndOf3 (_,b,_) = b
        rightTurn (unp2 -> (x1,y1)) (unp2 -> (x2, y2)) (unp2 -> (x3,y3)) =
          (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3-x1) < 0

------------------------------------------------------------
-- Function graphs
------------------------------------------------------------

-- $graphs
-- These functions are used to implement 'star', but are exported on
-- the offchance that someone else finds them useful.

-- | Pieces of a function graph can either be cycles or \"hairs\".
data GraphPart a = Cycle [a]
                 | Hair  [a]
  deriving (Show, Functor)

-- | @orbits f n@ computes the graph of @f@ on the integers mod @n@.
orbits :: (Int -> Int) -> Int -> [GraphPart Int]
orbits f n = runST genOrbits
  where
    f_n i = f i `mod` n

    genOrbits :: ST s [GraphPart Int]
    genOrbits = newArray (0,n-1) False >>= genOrbits'

    genOrbits' :: STUArray s Int Bool -> ST s [GraphPart Int]
    genOrbits' marks = liftM (concat . catMaybes) (forM [0 .. n-1] (genPart marks))

    genPart :: STUArray s Int Bool -> Int -> ST s (Maybe [GraphPart Int])
    genPart marks i = do
      tr <- markRho i marks
      case tr of
        [] -> return Nothing
        _  -> return . Just . splitParts $ tr

    markRho :: Int -> STUArray s Int Bool -> ST s [Int]
    markRho i marks = do
      isMarked <- readArray marks i
      case isMarked of
        True  -> return []
        False -> writeArray marks i True >>
                 liftM (i:) (markRho (f_n i) marks)

    splitParts :: [Int] -> [GraphPart Int]
    splitParts tr = hair ++ cyc
      where hair | not (null tl)   = [Hair $ tl ++ [f_n (last tl)]]
                 | otherwise       = []
            cyc  | not (null body) = [Cycle body]
                 | otherwise       = []
            l            = last tr
            (tl, body) = span (/= f_n l) tr

-- | Generate a function graph from the given function and labels.
mkGraph :: (Int -> Int) -> [a] -> [GraphPart a]
mkGraph f xs = (map . fmap) (xs!!) $ orbits f (length xs)

------------------------------------------------------------
--  Star polygons
------------------------------------------------------------

-- | Options for creating \"star\" polygons, where the edges connect
--   possibly non-adjacent vertices.
data StarOpts = StarFun (Int -> Int)
                -- ^ Specify the order in which the vertices should be
                --   connected by a function that maps each vertex
                --   index to the index of the vertex that should come
                --   next.  Indexing of vertices begins at 0.

              | StarSkip Int
                -- ^ Specify a star polygon by a \"skip\".  A skip of
                --   1 indicates a normal polygon, where edges go
                --   between successive vertices.  A skip of 2 means
                --   that edges will connect every second vertex,
                --   skipping one in between.  Generally, a skip of
                --   /n/ means that edges will connect every /n/th
                --   vertex.

-- | Create a generalized /star/ /polygon/.  The 'StarOpts' are used
--   to determine in which order the given vertices should be
--   connected.  The intention is that the second argument of type
--   @[P2]@ could be generated by a call to 'polygon', 'regPoly', or
--   the like, since a list of vertices is 'PathLike'.  But of course
--   the list can be generated any way you like.  A @'Path' 'R2'@ is
--   returned (instead of any 'PathLike') because the resulting path
--   may have more than one component, for example if the vertices are
--   to be connected in several disjoint cycles.
star :: StarOpts -> [P2] -> Path R2
star sOpts vs = graphToPath $ mkGraph f vs
  where f = case sOpts of
              StarFun g  -> g
              StarSkip k -> (+k)
        graphToPath = mconcat . map partToPath
        partToPath (Cycle ps) = close $ fromVertices ps
        partToPath (Hair ps)  = fromVertices ps
