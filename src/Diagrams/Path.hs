{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Path
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Generic functionality for constructing and manipulating /paths/
-- (sequences of linear or cubic Bezier segments) and related objects.
--
-----------------------------------------------------------------------------

module Diagrams.Path
       (
         -- * The 'Path' type

         Path(..)

         -- * Constructing paths

       , pathFromVertices
       , pathFromOffsets

       , close, open

       , stroke

         -- * Computing with paths

       , pathVertices
       , pathOffset
       , pathBounds

       ) where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Diagrams (rebaseBounds)
import Graphics.Rendering.Diagrams.Transform (HasLinearMap)

import Diagrams.Segment

import Data.VectorSpace

import Data.Monoid
import qualified Data.Map as M

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

------------------------------------------------------------
--  Paths  -------------------------------------------------
------------------------------------------------------------

-- | A /path/ is a sequence of segments placed end-to-end, together
--   with a distinguished starting point.  Hence, paths are /not/
--   translationally invariant.  Paths can also be /open/ or /closed/.
data Path v = Path { isClosed     :: Bool
                   , pathStart    :: Point v
                   , pathSegments :: [Segment v]
                   }
  deriving (Show, Functor)

-- | Paths form a monoid under path concatenation.  @p1 `mappend` p2@
--   is the path formed by taking the starting point of @p1@ and
--   appending the list of @p2@'s segments onto the end of @p1@'s list
--   (discarding the starting point of @p2@) --- unless @p1@ is the
--   empty path with no segments, in which case @p1 `mappend` p2@ is
--   just @p2@.  @p1 `mappend` p2@ is closed iff either @p1@ or @p2@
--   are.
instance (AdditiveGroup v) => Monoid (Path v) where
  mempty = Path False origin []
  Path c1 _ []     `mappend` Path c2 st segs = Path (c1 || c2) st segs
  Path c1 st segs1 `mappend` Path c2 _ segs2 = Path (c1 || c2) st (segs1 ++ segs2)

-- See Note [Transforming paths]
instance HasLinearMap v => Transformable (Path v) where
  type TSpace (Path v) = v
  transform t (Path c st segs) = Path c (transform t st) (transform t segs)

{- ~~~~ Note [Transforming paths]

Careful!  It's tempting to just define

> transform = fmap . transform

but that doesn't take into account the fact that some
of the v's are inside Points and hence ought to be translated.
-}

------------------------------------------------------------
--  Constructing paths  ------------------------------------
------------------------------------------------------------

-- | Build an open path of linear segments from a list of vertices.
pathFromVertices :: AdditiveGroup v => [Point v] -> Path v
pathFromVertices [] = mempty
pathFromVertices vvs@(v:vs)
  = Path { isClosed     = False
         , pathStart    = v
         , pathSegments = map Linear $ zipWith (flip (.-.)) vvs vs
         }

-- | Build an open path of linear segments from a starting location
--   and a list of offsets.
pathFromOffsets :: Point v -> [v] -> Path v
pathFromOffsets st segs = Path { isClosed     = False
                               , pathStart    = st
                               , pathSegments = map Linear segs
                               }

-- | Close a path.
close :: Path v -> Path v
close p = p { isClosed = True }

-- | Open a path.
open :: Path v -> Path v
open p = p { isClosed = False }

-- | Convert a path into a diagram.  The resulting diagram has the
--   names 0, 1, ... assigned to each of the path's vertices.
stroke :: ( v ~ BSpace b, TSpace v ~ v
          , InnerSpace v, AdditiveGroup (Scalar v), Ord (Scalar v), Floating (Scalar v)
          , Renderable (Path v) b)
       => Path v -> Diagram b
stroke p = Diagram { prims  = prim p
                   , bounds = pathBounds p
                   , names  = fromNames $ zip ([0..] :: [Int])
                                              (pathVertices p)  -- XXX names for Bezier
                                                                --   control points too?
                   , sample = const (Any False)   -- Paths are infinitely thin
                              -- TODO: what about closed paths in 2D?
                   }

------------------------------------------------------------
--  Computing with paths  ----------------------------------
------------------------------------------------------------

-- | Extract the vertices of a path.
pathVertices :: AdditiveGroup v => Path v -> [Point v]
pathVertices (Path _ st segs) = scanl (.+^) st (map segOffset segs)

-- | Compute the total offset from the start of a path to the end.
pathOffset :: (AdditiveGroup v) => Path v -> v
pathOffset = sumV . map segOffset . pathSegments

{- XXX TODO:
     - total arc length of a path
     - path splitting methods?
-}

-- | Compute the bounding function for an entire path.
pathBounds :: (InnerSpace v, AdditiveGroup (Scalar v), Floating (Scalar v), Ord (Scalar v))
           => Path v -> Bounds v
pathBounds (Path _ st segs) = rebaseBounds ((-1) *. st) $
  foldr (\seg bds -> rebaseBounds (P $ negateV (segOffset seg)) bds
                     <> segmentBounds seg)
        mempty
        segs

