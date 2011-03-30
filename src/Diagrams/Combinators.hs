{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , UndecidableInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Combinators
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Higher-level tools for combining diagrams.
--
-----------------------------------------------------------------------------

module Diagrams.Combinators where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform (HasLinearMap)
import Graphics.Rendering.Diagrams.Bounds (OrderedField)

import Diagrams.Segment (Segment(..))
import Diagrams.Path
import Diagrams.Align
import Diagrams.Util

import Data.AdditiveGroup
import Data.VectorSpace

import Data.Monoid
import Data.List

-- | Use the bounding region from some boundable object as the
--   bounding region for a diagram, in place of the diagram's default
--   bounding region.
withBounds :: Boundable a v => a -> AnnDiagram b v m -> AnnDiagram b v m
withBounds b d = d { bounds_ = bounds b }

------------------------------------------------------------
-- Combining two objects
------------------------------------------------------------

-- | Place two bounded, monoidal objects (i.e. diagrams or paths) next
--   to each other along the given vector.  In particular, place the
--   first object so that the vector points from its local origin to
--   the local origin of the second object, at a distance so that
--   their bounding regions are just tangent.  The local origin of the
--   new, combined object is at the point of tangency, along the line
--   between the old local origins.
--
--   XXX picture
beside :: (HasOrigin a v, Boundable a v, Monoid a) => v -> a -> a -> a
beside v d1 d2
  = align v d1 <> align (negateV v) d2

------------------------------------------------------------
-- Combining multiple objects
------------------------------------------------------------

-- | Combine a list of objects (i.e. diagrams or paths) by assigning
--   them absolute positions in the vector space of the combined
--   object.
position :: (HasOrigin a v, Monoid a) => [(Point v, a)] -> a
position = mconcat . map (uncurry moveTo)

-- | Combine a list of diagrams (or paths) by using them to
-- \"decorate\" a trail, placing the local origin of one diagram at
-- each successive vertex.  XXX say more
decorateTrail :: (HasOrigin a v, Monoid a) => Trail v -> [a] -> a
decorateTrail t = position . zip (trailVertices origin t)

cat :: (HasOrigin a v, Boundable a v, Monoid a) => v -> [a] -> a
cat v = foldl' (beside v) mempty

-- grid