{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}

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

        , polygon
        , polyTrail

        -- ** Generating polygon vertices

        , polyPolarTrail
        , polySidesTrail
        , polyRegularTrail

        , orient

        -- * Star polygons
        , StarOpts(..)
        , star

        -- ** Function graphs
        -- $graphs
        , GraphPart(..)
        , orbits, mkGraph

    ) where

import           Control.Monad           (forM, liftM)
import           Control.Monad.ST        (ST, runST)
import           Data.Array.ST           (STUArray, newArray, readArray,
                                          writeArray)
import           Data.List               (maximumBy, minimumBy)
import           Data.Maybe              (catMaybes)
import           Data.Monoid             (mconcat, mempty)
import           Data.Ord                (comparing)

import           Data.AffineSpace        ((.+^), (.-.))
import           Data.Default.Class
import           Data.VectorSpace        (magnitude, normalized, project, (<.>),
                                          (^*))

import           Diagrams.Core
import           Diagrams.Located
import           Diagrams.Path
import           Diagrams.Points         (centroid)
import           Diagrams.Trail
import           Diagrams.TrailLike
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector    (leftTurn, unitX, unitY, unit_Y)
import           Diagrams.Util           (tau, ( # ))

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
                   { polyType   :: PolyType
                     -- ^ Specification for the polygon's vertices.

                   , polyOrient :: PolyOrientation
                     -- ^ Should a rotation be applied to the
                     --   polygon in order to orient it in a
                     --   particular way?

                   , polyCenter :: P2
                     -- ^ Should a translation be applied to the
                     --   polygon in order to place the center at a
                     --   particular location?
                   }

-- | The default polygon is a regular pentagon of radius 1, centered
--   at the origin, aligned to the x-axis.
instance Default PolygonOpts where
    def = PolygonOpts (PolyRegular 5 1) OrientH origin

-- | Generate a polygon.  See 'PolygonOpts' for more information.
polyTrail :: PolygonOpts -> Located (Trail R2)
polyTrail po = transform ori tr
    where
        tr = case polyType po of
            PolyPolar ans szs -> polyPolarTrail ans szs
            PolySides ans szs -> polySidesTrail ans szs
            PolyRegular n r   -> polyRegularTrail n r
        ori = case polyOrient po of
            OrientH      -> orient unit_Y tr
            OrientV      -> orient unitX  tr
            OrientTo v   -> orient v      tr
            NoOrient     -> mempty

-- | Generate the polygon described by the given options.
polygon :: (TrailLike t, V t ~ R2) => PolygonOpts -> t
polygon = trailLike . polyTrail

-- | Generate the located trail of a polygon specified by polar data
--   (central angles and radii). See 'PolyPolar'.
polyPolarTrail :: Angle a => [a] -> [Double] -> Located (Trail R2)
polyPolarTrail [] _ = emptyTrail `at` origin
polyPolarTrail _ [] = emptyTrail `at` origin
polyPolarTrail ans (r:rs) = tr `at` p1
  where
    p1 = p2 (1,0) # scale r
    tr = closeTrail . trailFromVertices $
           zipWith
             (\a l -> rotate a . scale l $ p2 (1,0))
             (scanl (+) 0 ans)
             (r:rs)

-- | Generate the vertices of a polygon specified by side length and
--   angles, and a starting point for the trail such that the origin
--   is at the centroid of the vertices.  See 'PolySides'.
polySidesTrail :: Angle a => [a] -> [Double] -> Located (Trail R2)
polySidesTrail ans ls = tr `at` (centroid ps # scale (-1))
  where
    ans'    = scanl (+) 0 ans
    offsets = zipWith rotate ans' (map (unitY ^*) ls)
    ps      = scanl (.+^) origin offsets
    tr      = closeTrail . trailFromOffsets $ offsets

-- | Generate the vertices of a regular polygon.  See 'PolyRegular'.
polyRegularTrail :: Int -> Double -> Located (Trail R2)
polyRegularTrail n r = polyPolarTrail
                         (take (n-1) . repeat $ (tau::Rad) / fromIntegral n)
                         (repeat r)

-- | Generate a transformation to orient a trail.  @orient v t@
--   generates the smallest rotation such that one of the segments
--   adjacent to the vertex furthest in the direction of @v@ is
--   perpendicular to @v@.
orient :: R2 -> Located (Trail R2) -> T2
orient v = orientPoints v . trailVertices

orientPoints :: R2 -> [P2] -> T2
orientPoints v xs = rotation a
  where
    (n1,x,n2) = maximumBy (comparing (distAlong v . sndOf3))
                  (zip3 (tail (cycle xs)) xs (last xs : init xs))
    distAlong w ((.-. origin) -> p) = signum (w <.> p) * magnitude (project w p)
    sndOf3 (_,b,_) = b
    a = minimumBy (comparing abs) . map (angleFromNormal . (.-. x)) $ [n1,n2]
    v' = normalized v
    angleFromNormal o
      | leftTurn o' v' = phi
      | otherwise      = negate phi
      where
        o' = normalized o
        theta = acos (v' <.> o')
        phi
          | theta <= tau/4 = Rad $ tau/4 - theta
          | otherwise      = Rad $ theta - tau/4

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
--   the like, since a list of vertices is 'TrailLike'.  But of course
--   the list can be generated any way you like.  A @'Path' 'R2'@ is
--   returned (instead of any 'TrailLike') because the resulting path
--   may have more than one component, for example if the vertices are
--   to be connected in several disjoint cycles.
star :: StarOpts -> [P2] -> Path R2
star sOpts vs = graphToPath $ mkGraph f vs
  where f = case sOpts of
              StarFun g  -> g
              StarSkip k -> (+k)
        graphToPath = mconcat . map partToPath

        partToPath (Cycle ps) = pathFromLocTrail
                              . mapLoc closeTrail
                              . fromVertices
                              $ ps

        partToPath (Hair ps)  = fromVertices ps
