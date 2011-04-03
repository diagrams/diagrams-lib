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
withBounds :: Boundable a => a -> AnnDiagram b (V a) m -> AnnDiagram b (V a) m
withBounds b d = d { bounds = getBounds b }

-- XXX should this retain all the names etc. of d?
-- | @phantom a@ produces a \"phantom\" diagram, which has the same
--   bounding region as @a@ but produces no output.
phantom :: (Backend b (V a), Boundable a, Monoid m) => a -> AnnDiagram b (V a) m
phantom d = withBounds d mempty

-- | @pad s@ \"pads\" a diagram, expanding its bounding region by a
--   factor of @s@.
pad :: (Boundable (AnnDiagram b v m), Backend b v)
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
beside :: (HasOrigin a, Boundable a, Monoid a) => V a -> a -> a -> a
beside v d1 d2
  = align v d1 <> align (negateV v) d2

-- | XXX comment me
besideBounds :: (HasOrigin a, Boundable a) => Bounds (V a) -> V a -> a -> a
besideBounds b v a
  = moveOriginBy (origin .-. boundary v b) (align (negateV v) a)

-- | Like 'beside', but the origin of the final combined object is the
--   origin of the first object.  So in a sense we are \"appending\"
--   the second object onto the first.
append :: (HasOrigin a, Boundable a, Monoid a) => V a -> a -> a -> a
append v d1 d2 = appends d1 [(v,d2)]

-- | XXX comment me
appends :: (HasOrigin a, Boundable a, Monoid a) => a -> [(V a,a)] -> a
appends d1 apps = d1 <> mconcat (map (uncurry (besideBounds b)) apps)
  where b = getBounds d1

------------------------------------------------------------
-- Combining multiple objects
------------------------------------------------------------

-- | Combine a list of objects (i.e. diagrams or paths) by assigning
--   them absolute positions in the vector space of the combined
--   object.
position :: (HasOrigin a, Monoid a) => [(Point (V a), a)] -> a
position = mconcat . map (uncurry moveTo)

-- | Combine a list of diagrams (or paths) by using them to
-- \"decorate\" a trail, placing the local origin of one diagram at
-- each successive vertex.  XXX say more
decorateTrail :: (HasOrigin a, Monoid a) => Trail (V a) -> [a] -> a
decorateTrail t = position . zip (trailVertices origin t)

-- | Methods for concatenating diagrams.
data CatMethod = Cat     -- ^ Normal catenation: simply put diagrams
                         --   next to one another.
               | Distrib -- ^ Distribution: place the local origins of diagrams
                         --   at regular intervals.

-- | Options for the 'cat' function.
data CatOpts v = CatOpts { catMethod       :: CatMethod
                             -- ^ Which 'CatMethod' should be used:
                             --   normal catenation (default), or
                             --   distribution?
                         , sep             :: Scalar v
                             -- ^ If catenation, how much separation should be
                             --   placed between successive diagrams (default: 0)?
                             --   This option is ignored when @catMethod = Distrib@.
                         , catOptsvProxy__ :: Proxy v
                             -- ^ This field exists solely to aid type inference;
                             --   please ignore it.
                         }

-- The reason the proxy field is necessary is that without it,
-- altering the sep field could theoretically change the type of a
-- CatOpts record.  This causes problems when writing an expression
-- like @with { sep = 10 }@, because knowing the type of the whole
-- expression does not tell us anything about the type of @with@, and
-- therefore the @Num (Scalar v)@ constraint cannot be satisfied.
-- Adding the Proxy field constrains the type of @with@ in @with {sep
-- = 10}@ to be the same as the type of the whole expression.

instance Num (Scalar v) => Default (CatOpts v) where
  def = CatOpts { catMethod       = Cat
                , sep             = 0
                , catOptsvProxy__ = Proxy
                }

-- | XXX comment me
cat :: (HasOrigin a, Boundable a, Monoid a) => V a -> [a] -> a
cat v = cat' v def

-- XXX fix so something more reasonable happens to the origin
-- | XXX comment me
cat' :: (HasOrigin a, Boundable a, Monoid a) => V a -> CatOpts (V a) -> [a] -> a
cat' v (CatOpts { catMethod = Cat, sep = s }) []     = mempty
cat' v (CatOpts { catMethod = Cat, sep = s }) [d]    = d
cat' v (CatOpts { catMethod = Cat, sep = s }) (d:ds) =
  foldl' (\d1 d2 ->
           align v d1 <> (besideBounds (getBounds (Linear (withLength s v))) v d2))
         d
         ds

cat' v (CatOpts { catMethod = Distrib }) ds =
  decorateTrail (fromOffsets (repeat v)) ds
  -- infinite trail, no problem for Haskell =)
