{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TrailLike
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The 'TrailLike' class abstracts over anything which can be
-- constructed from a concretely located 'Trail', including
-- lines, loops, trails, paths, vertex lists, and diagrams.
--
-----------------------------------------------------------------------------

module Diagrams.TrailLike
       (
         -- * The TrailLike class

         TrailLike(..)

         -- * Constructing TrailLikes

       , fromSegments, fromLocSegments, fromOffsets, fromLocOffsets, fromVertices
       , (~~), explodeTrail

       ) where

import           Data.AffineSpace ((.-.))
import           Data.VectorSpace

import           Diagrams.Core
import           Diagrams.Located
import           Diagrams.Segment
import           Diagrams.Trail

------------------------------------------------------------
--  TrailLike class
------------------------------------------------------------

-- | A type class for trail-like things, /i.e./ things which can be
--   constructed from a concretely located 'Trail'.  Instances include
--   lines, loops, trails, paths, lists of vertices, two-dimensional
--   'Diagram's, and 'Located' variants of all the above.
--
--   Usually, type variables with 'TrailLike' constraints are used as
--   the /output/ types of functions, like
--
--   > foo :: (TrailLike t) => ... -> t
--
--   Functions with such a type can be used to construct trails,
--   paths, diagrams, lists of points, and so on, depending on the
--   context.
--
--   To write a function with a signature like the above, you can of
--   course call 'trailLike' directly; more typically, one would use
--   one of the provided functions like 'fromOffsets', 'fromVertices',
--   'fromSegments', or '~~'.
class (InnerSpace (V t), OrderedField (Scalar (V t))) => TrailLike t where

  trailLike
    :: Located (Trail (V t))  -- ^ The concretely located trail.  Note
                              --   that some trail-like things
                              --   (e.g. 'Trail's) may ignore the
                              --   location.
    -> t

------------------------------------------------------------
--  Instances  ---------------------------------------------

-- | A list of points is trail-like; this instance simply
--   computes the vertices of the trail, using 'trailVertices'.
instance (InnerSpace v, OrderedField (Scalar v)) => TrailLike [Point v] where
  trailLike = trailVertices

-- | Lines are trail-like.  If given a 'Trail' which contains a loop,
--   the loop will be cut with 'cutLoop'.  The location is ignored.
instance (InnerSpace v, OrderedField (Scalar v)) => TrailLike (Trail' Line v) where
  trailLike = withTrail id cutLoop . unLoc

-- | Loops are trail-like.  If given a 'Trail' containing a line, the
--   line will be turned into a loop using 'glueLine'.  The location
--   is ignored.
instance (InnerSpace v, OrderedField (Scalar v)) => TrailLike (Trail' Loop v) where
  trailLike = withTrail glueLine id . unLoc

-- | 'Trail's are trail-like; the location is simply ignored.
instance (InnerSpace v, OrderedField (Scalar v)) => TrailLike (Trail v) where
  trailLike = unLoc

-- | Translationally invariant things are trail-like as long as the
--   underlying type is.
instance TrailLike t => TrailLike (TransInv t) where
  trailLike = TransInv . trailLike

-- | 'Located' things are trail-like as long as the underlying type
--   is.  The location is taken to be the location of the input
--   located trail.
instance TrailLike t => TrailLike (Located t) where
  trailLike t = trailLike t `at` loc t

------------------------------------------------------------
--  Constructing TrailLike things  -------------------------
------------------------------------------------------------

-- | Construct a trail-like thing from a list of segments, with the
--   origin as the location.
--
--   XXX example/picture
fromSegments :: TrailLike t => [Segment Closed (V t)] -> t
fromSegments = fromLocSegments . (`at` origin)

-- | Construct a trail-like thing from a located list of segments.
fromLocSegments :: TrailLike t => Located [Segment Closed (V t)] -> t
fromLocSegments = trailLike . mapLoc trailFromSegments

-- | Construct a trail-like thing of linear segments from a list
--   of offsets, with the origin as the location.
--
--   XXX example/picture
fromOffsets :: TrailLike t => [V t] -> t
fromOffsets = trailLike . (`at` origin) . trailFromOffsets

-- | Construct a trail-like thing of linear segments from a located
--   list of offsets.
fromLocOffsets :: (V (V t) ~ V t, TrailLike t) => Located [V t] -> t
fromLocOffsets = trailLike . mapLoc trailFromOffsets

-- | Construct a trail-like thing connecting the given vertices with
--   linear segments, with the first vertex as the location.  If no
--   vertices are given, the empty trail is used with the origin as
--   the location.
--
--   XXX example/picture
fromVertices :: TrailLike t => [Point (V t)] -> t
fromVertices []       = trailLike (emptyTrail `at` origin)
fromVertices ps@(p:_) = trailLike (trailFromSegments (segmentsFromVertices ps) `at` p)

segmentsFromVertices :: AdditiveGroup v => [Point v] -> [Segment Closed v]
segmentsFromVertices []         = []
segmentsFromVertices vvs@(_:vs) = map straight (zipWith (flip (.-.)) vvs vs)

-- | Create a linear trail between two given points.
(~~) :: TrailLike t => Point (V t) -> Point (V t) -> t
p1 ~~ p2 = fromVertices [p1, p2]

-- | Given a concretely located trail, \"explode\" it by turning each
--   segment into its own separate trail.  Useful for (say) applying a
--   different style to each segment.
explodeTrail :: (VectorSpace (V t), TrailLike t) => Located (Trail (V t)) -> [t]
explodeTrail = map (mkTrail . fromFixedSeg) . fixTrail
  where
    mkTrail = trailLike . mapLoc (trailFromSegments . (:[]))
