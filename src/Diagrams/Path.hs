{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , UndecidableInstances
           , DeriveFunctor
           , GeneralizedNewtypeDeriving
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Path
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Generic functionality for constructing and manipulating /paths/
-- (sequences of linear or cubic Bezier segments) and related objects.
--
-----------------------------------------------------------------------------

module Diagrams.Path
       (
         -- * Path-like things

         PathLike(..), fromOffsets, fromVertices

         -- * Trails

       , Trail(..)

         -- ** Destructing trails

       , trailOffsets, trailOffset
       , trailVertices

         -- * Paths

       , Path(..)

         -- ** Constructing paths

       , pathFromTrail
       , pathFromTrailAt

         -- ** Computing with paths

       , pathVertices

         -- * Constructing path-based diagrams

       , stroke, strokeT

       ) where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform (HasLinearMap)
import Graphics.Rendering.Diagrams.Bounds (OrderedField)

import Diagrams.Segment

import Data.VectorSpace

import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Arrow ((***))

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

------------------------------------------------------------
--  PathLike class
------------------------------------------------------------

-- | Type class for path-like things, which must be monoids.
--   Instances include 'Trail's, 'Path's, and 'MultiPath's.
class Monoid p => PathLike p where
  type PathSpace p :: *

  -- | Set the starting point of the path-like thing.  Some path-like
  --   things (e.g. 'Trail's) may ignore this operation.
  setStart   :: Point (PathSpace p)   -> p -> p

  -- | Construct a path-like thing from a list of 'Segment's.
  fromSegments :: [Segment (PathSpace p)] -> p

  -- | \"Close\" a path-like thing.
  close :: p -> p

  -- | \"Open\" a path-like thing.
  open  :: p -> p

-- | Construct a path-like thing of linear segments from a list of
--   offsets.
fromOffsets :: PathLike p => [PathSpace p] -> p
fromOffsets = fromSegments . map Linear

-- | Construct a path-like thing of linear segments from a list of
--   vertices, with the first vertex as the starting point.
fromVertices :: (PathLike p, AdditiveGroup (PathSpace p))
             => [Point (PathSpace p)] -> p
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

-- | The empty trail has no segments.  Trails are composed via
--   concatenation.  @t1 `mappend` t2@ is closed iff either @t1@ or
--   @t2@ are.
instance Monoid (Trail v) where
  mempty = Trail [] False
  Trail t1 c1 `mappend` Trail t2 c2 = Trail (t1 ++ t2) (c1 || c2)

-- | Trails are 'PathLike' things.  Note that since trails are
--   translationally invariant, 'setStart' has no effect.
--   'fromSegments' creates an open trail.
instance PathLike (Trail v) where
  type PathSpace (Trail v) = v

  setStart _ tr     = tr
  fromSegments segs = Trail segs False
  close tr          = tr { isClosed = True }
  open tr           = tr { isClosed = False }

instance HasLinearMap v => Transformable (Trail v) where
  type TSpace (Trail v) = v
  transform t (Trail segs c) = Trail (transform t segs) c

-- | The bounding function for a trail is based at the trail's start.
instance (InnerSpace v, OrderedField (Scalar v)) => Boundable (Trail v) where
  type BoundSpace (Trail v) = v

  bounds (Trail segs _) =
    foldr (\seg bds -> moveOriginTo (P . negateV . segOffset $ seg) bds <> bounds seg)
          mempty
          segs

------------------------------------------------------------
--  Computing with trails  ---------------------------------
------------------------------------------------------------

-- | Extract the offsets of the segments of a trail.
trailOffsets :: Trail v -> [v]
trailOffsets (Trail segs _) = map segOffset segs

-- | Compute the total offset from the start of a trail to the end.
trailOffset :: AdditiveGroup v => Trail v -> v
trailOffset = sumV . trailOffsets

-- | Extract the vertices of a trail given a starting point.
trailVertices :: AdditiveGroup v => Point v -> Trail v -> [Point v]
trailVertices p = scanl (.+^) p . trailOffsets

------------------------------------------------------------
--  Paths  -------------------------------------------------
------------------------------------------------------------

-- | A /path/ is a (possibly empty) collection of trails, with each
--   trail paired with an absolute starting point. Hence, paths
--   are /not/ translationally invariant, and form a monoid under
--   union\/superposition.
newtype Path v = Path { pathTrails :: S.Set (Trail v, Point v) }
  deriving (Show, Monoid, Eq, Ord)

instance (Ord v, VectorSpace v) => HasOrigin (Path v) where
  type OriginSpace (Path v) = v
  moveOriginTo p (Path s) = Path $ S.map (id *** moveOriginTo p) s

-- | Paths are (of course) path-like. 'fromSegments' creates a path
--   with start point at the origin.
instance (Ord v, VectorSpace v) => PathLike (Path v) where
  type PathSpace (Path v) = v

  setStart = moveTo

  fromSegments []   = Path $ S.empty
  fromSegments segs = Path $ S.singleton (fromSegments segs, origin)

  close (Path s) = Path $ S.map (close *** id) s
  open  (Path s) = Path $ S.map (open  *** id) s

-- See Note [Transforming paths]
instance (HasLinearMap v, Ord v) => Transformable (Path v) where
  type TSpace (Path v) = v
  transform t (Path s) = Path $ S.map (transform t *** transform t) s

{- ~~~~ Note [Transforming paths]

Careful!  It's tempting to just define

> transform = fmap . transform

but that doesn't take into account the fact that some
of the v's are inside Points and hence ought to be translated.
-}

instance (InnerSpace v, OrderedField (Scalar v)) => Boundable (Path v) where
  type BoundSpace (Path v) = v

  bounds (Path trs) =  F.foldMap trailBounds trs
    where trailBounds (t, p) = moveOriginTo ((-1) *. p) (bounds t)

------------------------------------------------------------
--  Constructing paths from trails  ------------------------
------------------------------------------------------------

-- | Convert a trail to a path beginning at the origin.
pathFromTrail :: AdditiveGroup v => Trail v -> Path v
pathFromTrail t = Path $ S.singleton (t, origin)

-- | Convert a trail to a path with a particular starting point.
pathFromTrailAt :: Trail v -> Point v -> Path v
pathFromTrailAt t p = Path $ S.singleton (t, p)

------------------------------------------------------------
--  Computing with paths  ----------------------------------
------------------------------------------------------------

-- | Extract the vertices of a path.
pathVertices :: (AdditiveGroup v, Ord v) => Path v -> S.Set [Point v]
pathVertices (Path trs) = S.map (\(tr, p) -> trailVertices p tr) trs

------------------------------------------------------------
--  Constructing path-based diagrams  ----------------------
------------------------------------------------------------

-- | Convert a path into a diagram.  The resulting diagram has the
--   names 0, 1, ... assigned to each of the path's vertices.
stroke :: ( v ~ BSpace b, s ~ Scalar v
          , InnerSpace v, Renderable (Path v) b
          , AdditiveGroup s, Ord s, Floating s
          )
       => Path v -> Diagram b
stroke p = Diagram { prims   = prim p
                   , bounds_ = bounds p
                   , names   = mempty
                          {-  XXX what to do here?
                              fromNames $ zip ([0..] :: [Int])
                                              (pathVertices p)  -- XXX names for Bezier
                                                                --   control points too?
                          -}
                   , sample  = const (Any False)   -- Paths are infinitely thin
                              -- TODO: what about closed paths in 2D?
                   }

-- | Combination of 'pathFromTrail' and 'stroke' for convenience.
strokeT :: ( v ~ BSpace b, s ~ Scalar v
           , InnerSpace v, Renderable (Path v) b
           , AdditiveGroup s, Ord s, Floating s
           )
        => Trail v -> Diagram b
strokeT = stroke . pathFromTrail
