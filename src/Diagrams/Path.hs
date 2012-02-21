{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , DeriveFunctor
           , GeneralizedNewtypeDeriving
           , UndecidableInstances
           , ScopedTypeVariables
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Path
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines /trails/ (translationally invariant sequences
-- of linear or cubic Bézier segments) and /paths/ (collections of
-- concretely located trails).  Trails and paths can be used for
-- drawing shapes, laying out other diagrams, clipping, and other
-- things.
--
-----------------------------------------------------------------------------

module Diagrams.Path
       (
         -- * Constructing path-like things

         PathLike(..), fromSegments, fromOffsets, fromVertices, segmentsFromVertices
       , pathLikeFromTrail

         -- * Closeable things

       , Closeable(..)

         -- * Trails

       , Trail(..)

         -- ** Computing with trails

       , trailSegments'
       , trailOffsets, trailOffset
       , trailVertices, reverseTrail
       , addClosingSegment
       , fixTrail

         -- * Paths

       , Path(..)

         -- ** Constructing paths from trails

       , pathFromTrail
       , pathFromTrailAt

         -- ** Computing with paths

       , pathVertices
       , pathOffsets
       , pathCentroid
       , expandPath
       , reversePath
       , fixPath

         -- * Miscellaneous

       , explodeTrail
       , explodePath
       , (~~)

       ) where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Points

import Diagrams.Align
import Diagrams.Segment
import Diagrams.Points
import Diagrams.Transform

import Data.VectorSpace
import Data.AffineSpace

import Control.Newtype hiding (under)
import Data.Semigroup
import qualified Data.Foldable as F

import Data.List (mapAccumL)

import Control.Arrow ((***), first, second)

------------------------------------------------------------
--  PathLike class
------------------------------------------------------------

-- | Type class for path-like things, which must be monoids.
--   Instances include 'Trail's, 'Path's, and two-dimensional 'Diagram's.
class (Monoid' p, VectorSpace (V p)) => PathLike p where

  pathLike :: Point (V p)      -- ^ The starting point of the
                               --   path.  Some path-like things
                               --   (e.g. 'Trail's) may ignore this.
           -> Bool             -- ^ Should the path be closed?
           -> [Segment (V p)]  -- ^ Segments of the path.
           -> p

-- | A list of points is path-like; this instance simply computes the
--   vertices of a path-like thing.
instance VectorSpace v => PathLike [Point v] where
  pathLike start cl segs = trailVertices start (pathLike start cl segs)

-- | Construct an open path-like thing with the origin as a starting
--   point.
fromSegments :: PathLike p => [Segment (V p)] -> p
fromSegments = pathLike origin False

-- | Construct an open path-like thing of linear segments from a list
--   of offsets.  The starting point is the origin.
fromOffsets :: PathLike p => [V p] -> p
fromOffsets = pathLike origin False . map Linear

-- | Construct a path-like thing of linear segments from a list of
--   vertices, with the first vertex as the starting point.
fromVertices :: PathLike p => [Point (V p)] -> p
fromVertices []         = mempty
fromVertices vvs@(v:_) = pathLike v False (segmentsFromVertices vvs)

-- | Construct a list of linear segments from a list of vertices.  The
--   input list must contain at least two points to generate a
--   non-empty list of segments.
segmentsFromVertices :: AdditiveGroup v => [Point v] -> [Segment v]
segmentsFromVertices [] = []
segmentsFromVertices vvs@(_:vs) = map Linear (zipWith (flip (.-.)) vvs vs)

------------------------------------------------------------
--  Closeable class
------------------------------------------------------------

-- | Path-like things that can be \"open\" or \"closed\".
class PathLike p => Closeable p where
  -- | \"Open\" a path-like thing.
  open  :: p -> p

  -- | \"Close\" a path-like thing, by implicitly connecting the
  --   endpoint(s) back to the starting point(s).
  close :: p -> p

instance VectorSpace v => Closeable (Trail v) where
  close (Trail segs _) = Trail segs True
  open  (Trail segs _) = Trail segs False

instance VectorSpace v => Closeable (Path v) where
  close = (over Path . map . second) close
  open  = (over Path . map . second) open

------------------------------------------------------------
--  Trails  ------------------------------------------------
------------------------------------------------------------

-- | A /trail/ is a sequence of segments placed end-to-end.  Trails
--   are thus translationally invariant, and form a monoid under
--   concatenation.  Trails can also be /open/ (the default) or
--   /closed/ (the final point in a closed trail is implicitly
--   connected back to the starting point).
data Trail v = Trail { trailSegments :: [Segment v]
                     , isClosed      :: Bool
                     }
  deriving (Show, Functor, Eq, Ord)

type instance V (Trail v) = v

instance Semigroup (Trail v) where
  Trail t1 c1 <> Trail t2 c2 = Trail (t1 ++ t2) (c1 || c2)

-- | The empty trail has no segments.  Trails are composed via
--   concatenation.  @t1 ``mappend`` t2@ is closed iff either @t1@ or
--   @t2@ are.
instance Monoid (Trail v) where
  mempty = Trail [] False
  mappend = (<>)

-- | Trails are 'PathLike' things.  Note that since trails are
--   translationally invariant, 'setStart' has no effect.
--   'fromSegments' creates an open trail.
instance VectorSpace v => PathLike (Trail v) where
  pathLike _ cl segs = Trail segs cl

instance HasLinearMap v => Transformable (Trail v) where
  transform t (Trail segs c) = Trail (transform t segs) c

-- | The envelope for a trail is based at the trail's start.
instance (InnerSpace v, OrderedField (Scalar v)) => Enveloped (Trail v) where

  getEnvelope (Trail segs _) =
    foldr (\seg bds -> moveOriginTo (P . negateV . segOffset $ seg) bds <> getEnvelope seg)
          mempty
          segs

instance HasLinearMap v => Renderable (Trail v) NullBackend where
  render _ _ = mempty

------------------------------------------------------------
--  Computing with trails  ---------------------------------
------------------------------------------------------------

-- | @trailSegments'@ is like 'trailSegments', but explicitly includes
--   the implicit closing segment at the end of the list for closed trails.
trailSegments' :: AdditiveGroup v => Trail v -> [Segment v]
trailSegments' t | isClosed t = trailSegments t
                                ++ [straight . negateV . trailOffset $ t]
                 | otherwise  = trailSegments t

-- | Extract the offsets of the segments of a trail.
trailOffsets :: Trail v -> [v]
trailOffsets (Trail segs _) = map segOffset segs

-- | Compute the offset from the start of a trail to the end.
trailOffset :: AdditiveGroup v => Trail v -> v
trailOffset = sumV . trailOffsets

-- | Extract the vertices of a trail, given a concrete location at
--   which to place the first vertex.
trailVertices :: AdditiveGroup v => Point v -> Trail v -> [Point v]
trailVertices p = scanl (.+^) p . trailOffsets

-- | Reverse a trail's direction of travel.
reverseTrail :: AdditiveGroup v => Trail v -> Trail v
reverseTrail t@(Trail {trailSegments = []}) = t
reverseTrail t@(Trail {trailSegments = ss})
  | isClosed t = t { trailSegments = straight (trailOffset t) : reverseSegs ss }
  | otherwise  = t { trailSegments = reverseSegs ss }
  where reverseSegs = fmap reverseSegment . reverse

-- | Reverse a trail with a fixed starting point.
reverseRootedTrail :: AdditiveGroup v => (Point v, Trail v) -> (Point v, Trail v)
reverseRootedTrail (p, t)
  | isClosed t = (p, reverseTrail t)
  | otherwise  = (p .+^ trailOffset t, reverseTrail t)

-- | Convert a trail to any path-like thing.  @pathLikeFromTrail@ is the
--   identity on trails.
pathLikeFromTrail :: PathLike p => Trail (V p) -> p
pathLikeFromTrail t = pathLike origin (isClosed t) (trailSegments t)

-- | If the trail is closed, this adds in the closing segment. Otherwise,
--   the trail is returned unmodified.
addClosingSegment :: AdditiveGroup v => Trail v -> Trail v
addClosingSegment t | isClosed t = Trail (trailSegments t ++ [closeSeg]) False
                    | otherwise = t
 where closeSeg = Linear . negateV $ trailOffset t 

-- | Convert a starting point and a trail into a list of fixed segments.
fixTrail :: AdditiveGroup v => Point v -> Trail v -> [FixedSegment v]
fixTrail start t = zipWith mkFixedSeg (trailVertices start t)
                                      (trailSegments $ addClosingSegment t)

------------------------------------------------------------
--  Paths  -------------------------------------------------
------------------------------------------------------------

-- | A /path/ is a (possibly empty) list of trails, with each
--   trail paired with an absolute starting point. Hence, paths
--   are /not/ translationally invariant, and form a monoid under
--   superposition.
newtype Path v = Path { pathTrails :: [(Point v, Trail v)] }
  deriving (Show, Semigroup, Monoid, Eq, Ord)

type instance V (Path v) = v

instance Newtype (Path v) [(Point v, Trail v)] where
  pack   = Path
  unpack = pathTrails

instance VectorSpace v => HasOrigin (Path v) where
  moveOriginTo = over Path . map . first . moveOriginTo

-- | Paths are (of course) path-like. 'fromSegments' creates a path
--   with start point at the origin.
instance VectorSpace v => PathLike (Path v) where
  pathLike s cl segs = Path [(s, pathLike origin cl segs)]

-- See Note [Transforming paths]
instance HasLinearMap v => Transformable (Path v) where
  transform t = (over Path . map) (transform t *** transform t)

{- ~~~~ Note [Transforming paths]

Careful!  It's tempting to just define

> transform = fmap . transform

but that doesn't take into account the fact that some
of the v's are inside Points and hence ought to be translated.
-}

instance (InnerSpace v, OrderedField (Scalar v)) => Enveloped (Path v) where
  getEnvelope = F.foldMap trailEnvelope . pathTrails
          -- this type signature is necessary to work around an apparent bug in ghc 6.12.1
    where trailEnvelope :: (Point v, Trail v) -> Envelope v
          trailEnvelope (p, t) = moveOriginTo ((-1) *. p) (getEnvelope t)

instance (InnerSpace v, OrderedField (Scalar v)) => Juxtaposable (Path v) where
  juxtapose = juxtaposeDefault

instance (InnerSpace v, OrderedField (Scalar v)) => Alignable (Path v) where
  alignBy = alignByDefault

instance HasLinearMap v => Renderable (Path v) NullBackend where
  render _ _ = mempty

------------------------------------------------------------
--  Constructing paths from trails  ------------------------
------------------------------------------------------------

-- | Convert a trail to a path beginning at the origin.
pathFromTrail :: AdditiveGroup v => Trail v -> Path v
pathFromTrail t = Path [(origin, t)]

-- | Convert a trail to a path with a particular starting point.
pathFromTrailAt :: Trail v -> Point v -> Path v
pathFromTrailAt t p = Path [(p, t)]

------------------------------------------------------------
--  Computing with paths  ----------------------------------
------------------------------------------------------------

-- | Extract the vertices of a path.
pathVertices :: AdditiveGroup v => Path v -> [[Point v]]
pathVertices = map (uncurry trailVertices) . pathTrails

-- | Compute the total offset of each trail comprising a path.
pathOffsets :: AdditiveGroup v => Path v -> [v]
pathOffsets = map (trailOffset . snd) . pathTrails

-- | Compute the /centroid/ of a path (/i.e./ the average of its
--   vertices).
pathCentroid :: (VectorSpace v, Fractional (Scalar v)) => Path v -> Point v
pathCentroid = centroid . concat . pathVertices

-- | Scale a path using its centroid (see 'pathCentroid') as the base
--   point for the scale.
expandPath :: (HasLinearMap v, VectorSpace v, Fractional (Scalar v), Eq (Scalar v))
           => Scalar v -> Path v -> Path v
expandPath d p = (scale d `under` translation (origin .-. pathCentroid p)) p

-- | Reverse the direction of all the component trails of a path.
reversePath :: AdditiveGroup v => Path v -> Path v
reversePath = (over Path . map) reverseRootedTrail

-- | Convert a path into a list of lists of 'FixedSegment's.
fixPath :: AdditiveGroup v => Path v -> [[FixedSegment v]]
fixPath = map (uncurry fixTrail) . unpack

------------------------------------------------------------
--  Other functions  ---------------------------------------
------------------------------------------------------------

-- | Given a starting point, \"explode\" a trail by turning each
--   segment (including the implicit closing segment, if the trail is
--   closed) into its own separate path.  Useful for (say) applying a
--   different style to each segment.
explodeTrail :: (VectorSpace (V p), PathLike p) => Point (V p) -> Trail (V p) -> [p]
explodeTrail start = snd . mapAccumL mkPath start . trailSegments'
  where mkPath p seg = (p .+^ segOffset seg, pathLike p False [seg])

-- | \"Explode\" a path by exploding every component trail (see 'explodeTrail').
explodePath :: (VectorSpace (V p), PathLike p) => Path (V p) -> [[p]]
explodePath = map (uncurry explodeTrail) . pathTrails

-- | Create a single-segment path between two given points.
(~~) :: PathLike p => Point (V p) -> Point (V p) -> p
p1 ~~ p2 = fromVertices [p1, p2]
