{-# LANGUAGE CPP       #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

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
        , PolygonOpts(..), polyType, polyOrient, polyCenter

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

import           Control.Lens            (Lens', generateSignatures, lensRules,
                                          makeLensesWith, view, (.~), (^.))
import           Control.Monad           (forM, liftM)
import           Control.Monad.ST        (ST, runST)
import           Data.Array.ST           (STUArray, newArray, readArray,
                                          writeArray)
import           Data.Default.Class
import           Data.List               (maximumBy, minimumBy)
import           Data.Maybe              (catMaybes)
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid             (mconcat, mempty)
#endif
import           Data.Ord                (comparing)

import           Diagrams.Angle
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

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

-- | Method used to determine the vertices of a polygon.
data PolyType n = PolyPolar [Angle n] [n]
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

              | PolySides [Angle n] [n]
                -- ^ A polygon determined by the distance between
                --   successive vertices and the external angles formed
                --   by each three successive vertices. In other
                --   words, a polygon specified by \"turtle
                --   graphics\": go straight ahead x1 units; turn by
                --   external angle a1; go straight ahead x2 units; turn by
                --   external angle a2; etc. The polygon will be centered
                --   at the /centroid/ of its vertices.
                --
                --   * The first argument is a list of /vertex/
                --     /angles/, giving the external angle at each vertex
                --     from the previous vertex to the next.  The
                --     first angle in the list is the external angle at
                --     the /second/ vertex; the first edge always starts
                --     out heading in the positive y direction from
                --     the first vertex.
                --
                --   * The second argument is a list of distances
                --     between successive vertices.
                --
                --   To construct an /n/-gon, use a list of /n-2/
                --   angles and /n-1/ edge lengths.  Extra angles or
                --   lengths are ignored.

              | PolyRegular Int n
                -- ^ A regular polygon with the given number of
                --   sides (first argument) and the given radius
                --   (second argument).

-- | Determine how a polygon should be oriented.
data PolyOrientation n = NoOrient        -- ^ No special orientation; the first
                                         --   vertex will be at (1,0).
                                         --   This is the default.
                       | OrientH         -- ^ Orient /horizontally/, so the
                                         --   bottommost edge is parallel to
                                         --   the x-axis.
                       | OrientV         -- ^ Orient /vertically/, so the
                                         --   leftmost edge is parallel to the
                                         --   y-axis.
                       | OrientTo (V2 n) -- ^ Orient so some edge is
                                         --   /facing/ /in/ /the/ /direction/
                                         --   /of/, that is, perpendicular
                                         --   to, the given vector.
                       deriving (Eq, Ord, Show, Read)

-- | Options for specifying a polygon.
data PolygonOpts n = PolygonOpts
                   { _polyType   :: PolyType n
                   , _polyOrient :: PolyOrientation n
                   , _polyCenter :: Point V2 n
                   }

makeLensesWith (generateSignatures .~ False $ lensRules) ''PolygonOpts

-- | Specification for the polygon's vertices.
polyType :: Lens' (PolygonOpts n) (PolyType n)

-- | Should a rotation be applied to the polygon in order to orient it in a
--   particular way?
polyOrient :: Lens' (PolygonOpts n) (PolyOrientation n)

-- | Should a translation be applied to the polygon in order to place the center
--   at a particular location?
polyCenter :: Lens' (PolygonOpts n) (Point V2 n)

-- | The default polygon is a regular pentagon of radius 1, centered
--   at the origin, aligned to the x-axis.
instance Num n => Default (PolygonOpts n) where
    def = PolygonOpts (PolyRegular 5 1) OrientH origin

-- | Generate a polygon.  See 'PolygonOpts' for more information.
polyTrail :: OrderedField n => PolygonOpts n -> Located (Trail V2 n)
polyTrail po = transform ori tr
    where
        tr = case po^.polyType of
            PolyPolar ans szs -> polyPolarTrail ans szs
            PolySides ans szs -> polySidesTrail ans szs
            PolyRegular n r   -> polyRegularTrail n r
        ori = case po^.polyOrient of
            OrientH      -> orient unit_Y tr
            OrientV      -> orient unitX  tr
            OrientTo v   -> orient v      tr
            NoOrient     -> mempty

-- | Generate the polygon described by the given options.
polygon :: (InSpace V2 n t, TrailLike t) => PolygonOpts n -> t
polygon = trailLike . polyTrail

-- | Generate the located trail of a polygon specified by polar data
--   (central angles and radii). See 'PolyPolar'.
polyPolarTrail :: OrderedField n =>  [Angle n] -> [n] -> Located (Trail V2 n)
polyPolarTrail [] _ = emptyTrail `at` origin
polyPolarTrail _ [] = emptyTrail `at` origin
polyPolarTrail ans (r:rs) = tr `at` p1
  where
    p1 = p2 (1,0) # scale r
    tr = closeTrail . trailFromVertices $
           zipWith
             (\a l -> rotate a . scale l $ p2 (1,0))
             (scanl (^+^) zero ans)
             (r:rs)

-- | Generate the vertices of a polygon specified by side length and
--   angles, and a starting point for the trail such that the origin
--   is at the centroid of the vertices.  See 'PolySides'.
polySidesTrail :: OrderedField n =>  [Angle n] -> [n] -> Located (Trail V2 n)
polySidesTrail ans ls = tr `at` (centroid ps # scale (-1))
  where
    ans'    = scanl (^+^) zero ans
    offsets = zipWith rotate ans' (map (unitY ^*) ls)
    ps      = scanl (.+^) origin offsets
    tr      = closeTrail . trailFromOffsets $ offsets

-- | Generate the vertices of a regular polygon.  See 'PolyRegular'.
polyRegularTrail :: OrderedField n =>  Int -> n -> Located (Trail V2 n)
polyRegularTrail n r = polyPolarTrail
                         (replicate (n - 1) $ fullTurn ^/ fromIntegral n)
                         (repeat r)

-- | Generate a transformation to orient a trail.  @orient v t@
--   generates the smallest rotation such that one of the segments
--   adjacent to the vertex furthest in the direction of @v@ is
--   perpendicular to @v@.
orient :: OrderedField n => V2 n -> Located (Trail V2 n) -> Transformation V2 n
orient v = orientPoints v . trailVertices

orientPoints :: OrderedField n => V2 n -> [Point V2 n] -> Transformation V2 n
orientPoints v xs = rotation a
  where
    (n1,x,n2) = maximumBy (comparing (distAlong v . sndOf3))
                  (zip3 (tail (cycle xs)) xs (last xs : init xs))
    distAlong w ((.-. origin) -> p) = signum (w `dot` p) * norm (project w p)
    sndOf3 (_,b,_) = b
    -- a :: Angle (Scalar v)
    a = minimumBy (comparing $ abs . view rad)
        . map (angleFromNormal . (.-. x)) $ [n1,n2]
    v' = signorm v
    -- angleFromNormal :: v -> Angle (Scalar v)
    angleFromNormal o
      | leftTurn o' v' = phi
      | otherwise      = negated phi
      where
        o' = signorm o
        theta = acos (v' `dot` o')
        -- phi :: Angle (Scalar v)
        phi
          | theta <= tau/4 = tau/4 - theta @@ rad
          | otherwise      = theta - tau/4 @@ rad

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
      if isMarked
        then return []
        else writeArray marks i True >>
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
--   @[Point v]@ could be generated by a call to 'polygon', 'regPoly', or
--   the like, since a list of vertices is 'TrailLike'.  But of course
--   the list can be generated any way you like.  A @'Path' 'v'@ is
--   returned (instead of any 'TrailLike') because the resulting path
--   may have more than one component, for example if the vertices are
--   to be connected in several disjoint cycles.
star :: OrderedField n => StarOpts -> [Point V2 n] -> Path V2 n
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
