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

-- XXX make an export list
module Diagrams.Combinators where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform (HasLinearMap, withLength)
import Graphics.Rendering.Diagrams.Bounds (OrderedField)

import Diagrams.Segment (Segment(..))
import Diagrams.Path
import Diagrams.Align
import Diagrams.Util

import Data.AdditiveGroup
import Data.AffineSpace ((.-.))
import Data.VectorSpace

import Data.Monoid
import Data.List

import Data.Default

------------------------------------------------------------
-- Working with bounds
------------------------------------------------------------

-- | Use the bounding region from some boundable object as the
--   bounding region for a diagram, in place of the diagram's default
--   bounding region.
withBounds :: Boundable a v => a -> AnnDiagram b v m -> AnnDiagram b v m
withBounds b d = d { bounds = getBounds b }

-- XXX should this retain all the names etc. of d?
-- | @phantom a@ produces a \"phantom\" diagram, which has the same
--   bounding region as @a@ but produces no output.
phantom :: (Backend b v, Boundable a v, Monoid m) => a -> AnnDiagram b v m
phantom d = withBounds d mempty

-- | @pad s@ \"pads\" a diagram, expanding its bounding region by a
--   factor of @s@.
pad :: (Boundable (AnnDiagram b v m) v, Backend b v)
    => Scalar v -> AnnDiagram b v m -> AnnDiagram b v m
pad s d = withBounds (d # scale s) d

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

-- | XXX comment me
besideBounds :: (HasOrigin a v, Boundable a v) => Bounds v -> v -> a -> a
besideBounds b v a
  = moveOriginBy (origin .-. boundary v b) (align (negateV v) a)

-- | Like 'beside', but the origin of the final combined object is the
--   origin of the first object.  So in a sense we are \"appending\"
--   the second object onto the first.
append :: (HasOrigin a v, Boundable a v, Monoid a) => v -> a -> a -> a
append v d1 d2 = appends d1 [(v,d2)]

-- | XXX comment me
appends :: (HasOrigin a v, Boundable a v, Monoid a) => a -> [(v,a)] -> a
appends d1 apps = d1 <> mconcat (map (uncurry (besideBounds b)) apps)
  where b = getBounds d1

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

-- | XXX comment me
data CatMethod = Cat | Distrib

-- | XXX comment me
data CatOpts v = CatOpts { catMethod :: CatMethod
                         , sep       :: Scalar v
                         }

instance Num (Scalar v) => Default (CatOpts v) where
  def = CatOpts { catMethod = Cat
                , sep       = 0
                }

-- | XXX comment me
cat :: (HasOrigin a v, Boundable a v, Monoid a) => v -> [a] -> a
cat v = cat' v def

-- XXX fix so something more reasonable happens to the origin
-- | XXX comment me
cat' :: (HasOrigin a v, Boundable a v, Monoid a) => v -> CatOpts v -> [a] -> a
cat' v (CatOpts { catMethod = Cat, sep = s }) =
  foldl' (\d1 d2 ->
           beside v d1 (besideBounds (getBounds (Linear (withLength s v))) v d2))
         mempty

cat' v (CatOpts { catMethod = Distrib }) =
  decorateTrail (fromOffsets (repeat v))  -- infinite trail, no problem for Haskell =)
