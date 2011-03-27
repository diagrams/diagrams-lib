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
         -- * Trails

         Trail(..)

         -- ** Constructing trails

       , emptyTrail
       , trailFromOffsets, trailFromVertices
       , closeTrail, openTrail

         -- ** Computing with trails

       , trailOffsets, trailOffset
       , trailVertices

         -- * Paths

       , Path(..)

         -- ** Constructing paths

       , emptyPath
       , pathFromVertices,  pathFromOffsets, pathFromTrailAt
       , close, open

         -- ** Computing with paths

       , pathVertices

         -- * Constructing path-based diagrams

       , stroke, strokeT

       ) where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform (HasLinearMap)

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
--  Trails  ------------------------------------------------
------------------------------------------------------------

-- | A /trail/ is a sequence of segments placed end-to-end.  Trails
--   are thus translationally invariant, and form a monoid under
--   concatenation.  Trails can also be /open/ (the default) or
--   /closed/ (the final point in the trail is implicitly connected
--   back to the starting point).

data Trail v = Trail { trailSegments :: [Segment v]
                     , isClosed      :: Bool
                     }
  deriving (Show, Functor, Eq, Ord)

-- | The empty trail, with no segments.
emptyTrail :: Trail v
emptyTrail = Trail [] False

-- | Trails are composed via concatenation.  @t1 `mappend` t2@ is
--   closed iff either @t1@ or @t2@ are.
instance Monoid (Trail v) where
  mempty = emptyTrail
  Trail t1 c1 `mappend` Trail t2 c2 = Trail (t1 ++ t2) (c1 || c2)

instance HasLinearMap v => Transformable (Trail v) where
  type TSpace (Trail v) = v
  transform t (Trail segs c) = Trail (transform t segs) c

-- | The bounding function for a trail is based at the trail's start.
instance ( s ~ Scalar v, Ord s, Floating s, AdditiveGroup s
         , InnerSpace v, HasLinearMap v )
         => Boundable (Trail v) where
  type BoundSpace (Trail v) = v

  bounds (Trail segs _) =
    foldr (\seg bds -> translate (segOffset seg) bds <> bounds seg)
          mempty
          segs

------------------------------------------------------------
--  Constructing trails  -----------------------------------
------------------------------------------------------------

-- | Build an open trail of linear segments from a list of offsets.
trailFromOffsets :: [v] -> Trail v
trailFromOffsets segs = Trail { trailSegments = map Linear segs
                              , isClosed      = False
                              }

-- | Build an open trail of linear segments from a list of vertices,
--   remembering only the relative positions of the points.
trailFromVertices :: AdditiveGroup v => [Point v] -> Trail v
trailFromVertices [] = mempty
trailFromVertices vvs@(v:vs)
  = Trail { trailSegments = map Linear $ zipWith (flip (.-.)) vvs vs
          , isClosed      = False
          }

-- | Close a trail.
closeTrail :: Trail v -> Trail v
closeTrail t = t { isClosed = True }

-- | Open a trail.
openTrail :: Trail v -> Trail v
openTrail t = t { isClosed = False }

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

-- | The empty path consisting of no trails.
emptyPath :: Path v
emptyPath = Path S.empty

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

instance ( s ~ Scalar v, Ord s, Floating s, AdditiveGroup s
         , InnerSpace v, HasLinearMap v)
         => Boundable (Path v) where
  type BoundSpace (Path v) = v

  bounds (Path trs) =  F.foldMap trailBounds trs
    where trailBounds (t, p) = translate (p .-. origin) (bounds t)
      -- XXX use moveOriginTo?  Can probably remove HasLinearMap in that case?

instance (Ord v, VectorSpace v) => HasOrigin (Path v) where
  type OriginSpace (Path v) = v
  moveOriginTo p (Path s) = Path $ S.map (id *** moveOriginTo p) s

------------------------------------------------------------
--  Constructing paths  ------------------------------------
------------------------------------------------------------

-- | Create a path with a single open trail of linear segments from a
--   list of vertices.
pathFromVertices :: (AdditiveGroup v, Ord v) => [Point v] -> Path v
pathFromVertices [] = mempty
pathFromVertices vvs@(v:vs) = Path $ S.singleton (trailFromVertices vvs, v)

-- | Create a path with a single open trail of linear segments from a
--   starting location and a list of offsets.
pathFromOffsets :: Point v -> [v] -> Path v
pathFromOffsets st segs = Path $ S.singleton (trailFromOffsets segs, st)

-- | Convert a trail to a path beginning at the origin.
pathFromTrail :: AdditiveGroup v => Trail v -> Path v
pathFromTrail t = Path $ S.singleton (t, origin)

-- | Convert a trail to a path with a particular starting point.
pathFromTrailAt :: Trail v -> Point v -> Path v
pathFromTrailAt t p = Path $ S.singleton (t, p)

-- | Close a path by closing all its constituent trails.
close :: Ord v => Path v -> Path v
close (Path s) = Path $ S.map (closeTrail *** id) s

-- | Open a path by closing all its constituent trails.
open :: Ord v => Path v -> Path v
open (Path s) = Path $ S.map (openTrail *** id) s


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
