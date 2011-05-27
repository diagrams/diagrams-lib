{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , DeriveFunctor
           , GeneralizedNewtypeDeriving
           , UndecidableInstances
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

         PathLike(..), fromOffsets, fromVertices

         -- * Trails

       , Trail(..)

         -- ** Destructing trails

       , trailOffsets, trailOffset
       , trailVertices, reverseTrail

         -- * Paths

       , Path(..)

         -- ** Constructing paths from trails

       , pathFromTrail
       , pathFromTrailAt

         -- ** Destructing paths

       , pathVertices

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Segment
import Diagrams.Util

import Data.VectorSpace
import Data.AffineSpace

import Data.Monoid
import qualified Data.Foldable as F

import Data.Tuple (swap)

import Control.Arrow ((***), first, second)

------------------------------------------------------------
--  PathLike class
------------------------------------------------------------

-- XXX merge close/open into fromSegments and make Diagrams an instance?

-- | Type class for path-like things, which must be monoids.
--   Instances include 'Trail's and 'Path's.
class (Monoid p, VectorSpace (V p)) => PathLike p where

  -- | Set the starting point of the path-like thing.  Some path-like
  --   things (e.g. 'Trail's) may ignore this operation.
  setStart   :: Point (V p) -> p -> p

  -- | Construct a path-like thing from a list of 'Segment's.
  fromSegments :: [Segment (V p)] -> p

  -- | \"Close\" a path-like thing, by implicitly connecting the
  --   endpoint(s) back to the starting point(s).
  close :: p -> p

  -- | \"Open\" a path-like thing.
  open  :: p -> p

-- | Construct a path-like thing of linear segments from a list of
--   offsets.
fromOffsets :: PathLike p => [V p] -> p
fromOffsets = fromSegments . map Linear

-- | Construct a path-like thing of linear segments from a list of
--   vertices, with the first vertex as the starting point.
fromVertices :: PathLike p => [Point (V p)] -> p
fromVertices []         = mempty
fromVertices vvs@(v:vs) = setStart v $ fromOffsets (zipWith (flip (.-.)) vvs vs)

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
  setStart _ tr     = tr
  fromSegments segs = Trail segs False
  close tr          = tr { isClosed = True }
  open tr           = tr { isClosed = False }

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
reverseTrail t = t { trailSegments = (fmap . fmap) negateV . reverse
                       $ trailSegments t
                   }

------------------------------------------------------------
--  Paths  -------------------------------------------------
------------------------------------------------------------

-- | A /path/ is a (possibly empty) list of trails, with each
--   trail paired with an absolute starting point. Hence, paths
--   are /not/ translationally invariant, and form a monoid under
--   superposition.
newtype Path v = Path { pathTrails :: [(Trail v, Point v)] }
  deriving (Show, Monoid, Eq, Ord)

type instance V (Path v) = v

inPath :: ([(Trail v, Point v)] -> [(Trail v, Point v)]) -> Path v -> Path v
inPath f (Path ts) = Path (f ts)

instance (Ord v, VectorSpace v) => HasOrigin (Path v) where
  moveOriginTo = inPath . map . second . moveOriginTo

-- | Paths are (of course) path-like. 'fromSegments' creates a path
--   with start point at the origin.
instance (Ord v, VectorSpace v) => PathLike (Path v) where
  setStart          = moveTo

  fromSegments []   = Path []
  fromSegments segs = Path [(fromSegments segs, origin)]

  close = (inPath . map . first) close
  open  = (inPath . map . first) open

-- See Note [Transforming paths]
instance (HasLinearMap v, Ord v) => Transformable (Path v) where
  transform t = (inPath . map) (transform t *** transform t)

{- ~~~~ Note [Transforming paths]

Careful!  It's tempting to just define

> transform = fmap . transform

but that doesn't take into account the fact that some
of the v's are inside Points and hence ought to be translated.
-}

instance (InnerSpace v, OrderedField (Scalar v)) => Boundable (Path v) where

  getBounds = F.foldMap trailBounds . pathTrails
    where trailBounds (t, p) = moveOriginTo ((-1) *. p) (getBounds t)

------------------------------------------------------------
--  Constructing paths from trails  ------------------------
------------------------------------------------------------

-- | Convert a trail to a path beginning at the origin.
pathFromTrail :: AdditiveGroup v => Trail v -> Path v
pathFromTrail t = Path [(t, origin)]

-- | Convert a trail to a path with a particular starting point.
pathFromTrailAt :: Trail v -> Point v -> Path v
pathFromTrailAt t p = Path [(t, p)]

------------------------------------------------------------
--  Destructing paths  -------------------------------------
------------------------------------------------------------

-- | Extract the vertices of a path.
pathVertices :: (AdditiveGroup v, Ord v) => Path v -> [[Point v]]
pathVertices = map (uncurry trailVertices . swap) . pathTrails
