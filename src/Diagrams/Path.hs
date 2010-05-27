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
         -- * Paths

         Path(..)

         -- * Computing with paths

       , pathOffset
       , pathBounds

       ) where

import Graphics.Rendering.Diagrams

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
                   , pathStart    :: v
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
  mempty = Path False zeroV []
  Path c1 _ []     `mappend` Path c2 st segs = Path (c1 || c2) st segs
  Path c1 st segs1 `mappend` Path c2 _ segs2 = Path (c1 || c2) st (segs1 ++ segs2)

instance (AdditiveGroup v, Transformable v) => Transformable (Path v) where
  type TSpace (Path v) = TSpace v
  transform = fmap . transform

------------------------------------------------------------
--  Computing with paths  ----------------------------------
------------------------------------------------------------

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
pathBounds (Path _ st segs) = rebaseBounds (negateV st) $
  foldr (\seg bds -> rebaseBounds (negateV (segOffset seg)) bds
                     <> segmentBounds seg)
        mempty
        segs

-- Build a zero-based path from a list of segments.
{-
path :: ( InnerSpace v, Floating (Scalar v), AdditiveGroup (Scalar v), Ord (Scalar v)
        , TSpace v ~ v)
     => (Renderable (Path v) b, BSpace b ~ v) => [v] -> Diagram b
path ss = Diagram [Prim (Path False zeroV (RelPath segs))]
                  pathBounds
                  mempty
  where pathBounds =
        segs = map Linear ss
-}