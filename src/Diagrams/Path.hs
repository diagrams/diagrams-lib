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
-- Generic functionality for constructing and manipulating /trails/
-- (sequences of linear or cubic Bezier segments) and /paths/
-- (collections of concretely located trails).
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
       , fixTrail

         -- * Paths

       , Path(..)

         -- ** Constructing paths from trails

       , pathFromTrail
       , pathFromTrailAt

         -- ** Computing with paths

       , pathVertices
       , pathOffsets
       , reversePath
       , fixPath

         -- * Miscellaneous

       , explodeTrail
       , explodePath
       , (~~)

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Segment
import Diagrams.Util

import Data.VectorSpace
import Data.AffineSpace

import Control.Newtype
import Data.Monoid
import qualified Data.Foldable as F

import Data.List (mapAccumL)

import Control.Arrow ((***), first, second)

------------------------------------------------------------
--  PathLike class
------------------------------------------------------------

-- | Type class for path-like things, which must be monoids.
--   Instances include 'Trail's, 'Path's, and two-dimensional 'Diagram's.
class (Monoid p, VectorSpace (V p)) => PathLike p where

  pathLike :: Point (V p)      -- ^ The starting point of the
                               --   path.  Some path-like things
                               --   (e.g. 'Trail's) may ignore this.
           -> Bool             -- ^ Should the path be closed?
           -> [Segment (V p)]  -- ^ Segments of the path.
           -> p

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

-- | Construct a list of segments from a (non-empty) list of vertices.
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

instance (VectorSpace v, Ord v) => Closeable (Path v) where
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

-- | The empty trail has no segments.  Trails are composed via
--   concatenation.  @t1 ``mappend`` t2@ is closed iff either @t1@ or
--   @t2@ are.
instance Monoid (Trail v) where
  mempty = Trail [] False
  Trail t1 c1 `mappend` Trail t2 c2 = Trail (t1 ++ t2) (c1 || c2)

-- | Trails are 'PathLike' things.  Note that since trails are
--   translationally invariant, 'setStart' has no effect.
--   'fromSegments' creates an open trail.
instance VectorSpace v => PathLike (Trail v) where
  pathLike _ cl segs = Trail segs cl

instance HasLinearMap v => Transformable (Trail v) where
  transform t (Trail segs c) = Trail (transform t segs) c

-- | The bounding function for a trail is based at the trail's start.
instance (InnerSpace v, OrderedField (Scalar v)) => Boundable (Trail v) where

  getBounds (Trail segs _) =
    foldr (\seg bds -> moveOriginTo (P . negateV . segOffset $ seg) bds <> getBounds seg)
          mempty
          segs

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
reverseTrail t@(Trail {trailSegments = sss@(_:ss)})
  | isClosed t = t { trailSegments = straight (trailOffset t) : reverseSegs ss }
  | otherwise  = t { trailSegments = reverseSegs $ sss }
  where reverseSegs = fmap reverseSegment . reverse

-- | Reverse a trail with a fixed starting point.
reverseRootedTrail :: AdditiveGroup v => (Point v, Trail v) -> (Point v, Trail v)
reverseRootedTrail (p, t)
  | isClosed t = (p .+^ trailOffset t, reverseTrail t)
  | otherwise  = (p, reverseTrail t)

-- | Convert a trail to any path-like thing.  @pathLikeFromTrail@ is the
--   identity on trails.
pathLikeFromTrail :: PathLike p => Trail (V p) -> p
pathLikeFromTrail t = pathLike origin (isClosed t) (trailSegments t)

-- | Convert a starting point and a trail into a list of fixed segments.
fixTrail :: AdditiveGroup v => Point v -> Trail v -> [FixedSegment v]
fixTrail start tr = zipWith mkFixedSeg (trailVertices start tr)
                      (trailSegments tr ++ closeSeg)
  where closeSeg | isClosed tr = [Linear . negateV . trailOffset $ tr]
                 | otherwise   = []

------------------------------------------------------------
--  Paths  -------------------------------------------------
------------------------------------------------------------

-- | A /path/ is a (possibly empty) list of trails, with each
--   trail paired with an absolute starting point. Hence, paths
--   are /not/ translationally invariant, and form a monoid under
--   superposition.
newtype Path v = Path { pathTrails :: [(Point v, Trail v)] }
  deriving (Show, Monoid, Eq, Ord)

type instance V (Path v) = v

instance Newtype (Path v) [(Point v, Trail v)] where
  pack   = Path
  unpack = pathTrails

instance (Ord v, VectorSpace v) => HasOrigin (Path v) where
  moveOriginTo = over Path . map . first . moveOriginTo

-- | Paths are (of course) path-like. 'fromSegments' creates a path
--   with start point at the origin.
instance (Ord v, VectorSpace v) => PathLike (Path v) where
  pathLike s cl segs = Path [(s, pathLike origin cl segs)]

-- See Note [Transforming paths]
instance (HasLinearMap v, Ord v) => Transformable (Path v) where
  transform t = (over Path . map) (transform t *** transform t)

{- ~~~~ Note [Transforming paths]

Careful!  It's tempting to just define

> transform = fmap . transform

but that doesn't take into account the fact that some
of the v's are inside Points and hence ought to be translated.
-}

instance (InnerSpace v, OrderedField (Scalar v)) => Boundable (Path v) where
  getBounds = F.foldMap trailBounds . pathTrails
          -- this type signature is necessary to work around an apparent bug in ghc 6.12.1
    where trailBounds :: (Point v, Trail v) -> Bounds v
          trailBounds (p, t) = moveOriginTo ((-1) *. p) (getBounds t)

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
explodeTrail :: VectorSpace v => Point v -> Trail v -> [Path v]
explodeTrail start = snd . mapAccumL mkPath start . trailSegments'
  where mkPath p seg = (p .+^ segOffset seg, pathFromTrailAt (fromSegments [seg]) p)

-- | \"Explode\" a path by exploding every component trail (see 'explodeTrail').
explodePath :: VectorSpace v => Path v -> [[Path v]]
explodePath = map (uncurry explodeTrail) . pathTrails

-- | Create a single-segment path between two given points.
(~~) :: PathLike p => Point (V p) -> Point (V p) -> p
p1 ~~ p2 = fromVertices [p1, p2]
