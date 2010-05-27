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
         -- * Relative paths

         RelPath(..)

       , relPathOffset

         -- * Based paths

       , Path(..)

       , path

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Segment

import Data.VectorSpace

import Data.Monoid
import qualified Data.Map as M

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

------------------------------------------------------------
--  Relative paths  ----------------------------------------
------------------------------------------------------------

-- | A /relative path/ is a sequence of segments placed end-to-end.
--   Like its constituent segments, a relative path is also
--   translationally invariant.
newtype RelPath v = RelPath { relPathSegments :: [Segment v]
                              -- ^ Get the list of segments comprising a relative path.
                            }
  deriving (Show, Functor)

-- | Relative paths form a monoid under path concatenation.
instance Monoid (RelPath v) where
  mempty = RelPath []
  (RelPath b1) `mappend` (RelPath b2) = RelPath (b1 ++ b2)

instance (AdditiveGroup v, Transformable v) => Transformable (RelPath v) where
  type TSpace (RelPath v)  = TSpace v
  transform = fmap . transform

-- | Compute the total offset from the start of a relative path to the
--   end.
relPathOffset :: (AdditiveGroup v) => RelPath v -> v
relPathOffset = sumV . map segOffset . relPathSegments

------------------------------------------------------------
--  Paths  -------------------------------------------------
------------------------------------------------------------

-- | A /path/ is a relative path together with a fixed starting point;
--   hence, paths are /not/ translationally invariant.
data Path v = Path { isClosed     :: Bool
                   , pathStart    :: v
                   , pathSegments :: (RelPath v)
                   }
  deriving (Show, Functor)

instance (AdditiveGroup v, Transformable v) => Transformable (Path v) where
  type TSpace (Path v)   = TSpace v
  transform = fmap . transform

-- Build a zero-based path from a list of segments.

path :: ( InnerSpace v, Floating (Scalar v), AdditiveGroup (Scalar v), Ord (Scalar v)
        , TSpace v ~ v)
     => (Renderable (Path v) b, BSpace b ~ v) => [v] -> Diagram b
path ss = Diagram [Prim (Path False zeroV (RelPath segs))]
                  pathBounds
                  mempty
  where pathBounds = foldr (\seg bds -> rebaseBounds (negateV (segOffset seg)) bds
                                        <> segmentBounds seg) mempty segs
        segs = map Linear ss