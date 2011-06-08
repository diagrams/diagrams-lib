{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , UndecidableInstances
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Align
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- General tools for alignment.  Any boundable object with a local
-- origin can be aligned; this includes diagrams, of course, but it also
-- includes paths.
--
-----------------------------------------------------------------------------

module Diagrams.Align
       ( -- * Generic alignment functions
         align, alignBy
       , center

         -- * Alignment objects
       , Alignment, asAlignment, runAlignment
       ) where

import Graphics.Rendering.Diagrams

import Data.Monoid
import Data.VectorSpace
import Data.AffineSpace (alerp, (.-.))

-- | @align v@ aligns a boundable object along the edge in the
--   direction of @v@.  That is, it moves the local origin in the
--   direction of @v@ until it is on the boundary.  (Note that if the
--   local origin is outside the boundary to begin, it may have to
--   move \"backwards\".)
align :: (HasOrigin a, Boundable a) => V a -> a -> a
align v a = moveOriginTo (boundary v a) a


-- | @align v d a@ moves the origin of @a@ along the vector @v@. If @d
--   = 1@, the origin is moved to the boundary in the direction of
--   @v@; if @d = -1@, it moves to the boundary in the direction of
--   the negation of @v@.  Other values of @d@ interpolate linearly
--   (so for example, @d = 0@ centers the origin along the direction
--   of @v@).
alignBy :: (HasOrigin a, Boundable a, Num (Scalar (V a)))
        => V a -> Scalar (V a) -> a -> a
alignBy v d a = moveOriginTo (alerp (boundary (negateV v) a)
                                    (boundary v a)
                                    ((d + 1) / 2))
                             a

-- | @center v@ centers a boundable object along the direction of @v@.
center :: (HasOrigin a, Boundable a) => V a -> a -> a
center v = alignBy v 0

------------------------------------------------------------
-- Alignment objects
------------------------------------------------------------

-- | An @Alignment@ value is a concrete first-order representation of
--   an alignment, i.e. a series of applications of alignment
--   functions such as 'align', 'alignBy', or more specific versions
--   in particular vector spaces (like 'alignB', 'alignTL', etc. for
--   2D).  An alignment function can be converted into an @Alignment@
--   value using 'asAlignment'; the inverse conversion is by
--   'runAlignment'.
newtype Alignment v = Alignment [v]

-- | 'Alignment' values form a monoid: the empty alignment is the one
--   which has no effect; @a2 \<\> a1@ is the alignment which performs
--   first @a1@, then @a2@.
instance Monoid (Alignment v) where
  mempty = Alignment []
  (Alignment as1) `mappend` (Alignment as2) = Alignment (as1 ++ as2)

type instance V (Alignment v) = v

instance (InnerSpace v, OrderedField (Scalar v)) => Boundable (Alignment v) where
    getBounds _ = Bounds $ (1/) . magnitude
    -- The bounds for @Alignment@ objects are always just a unit
    -- circle.  This way, performing an alignment on an @Alignment@
    -- object will always result in translating its origin by some
    -- fraction of a unit vector.

instance VectorSpace v => HasOrigin (Alignment v) where
  moveOriginTo p (Alignment vs) = Alignment ((origin .-. p) : vs)
  -- Any alignment is actually carried out by a call to moveOriginTo;
  -- hence the moveOriginTo implementation for @Alignment@ simply
  -- causes the translation to be recorded, so we can replay the
  -- alignments later.

-- | @asAlignment@ converts any alignment function into an
--   'Alignment'.  The type may look a bit strange, but since
--   'Alignment' is an instance of both 'HasOrigin' and 'Boundable',
--   the idea is that @asAlignment@ can be used with any \"alignment
--   function\" of type
--
--   > (HasOrigin a, Boundable a) => a -> a
--
--   such as @'align' v@, @'alignBy' v s@, 'alignB', 'alignTL', ...
asAlignment :: AdditiveGroup v => (Alignment v -> Alignment v) -> Alignment v
asAlignment f = f (Alignment [])

-- | \"Run\" an 'Alignment' by converting it back into a function on
--   boundable things with origins.  @runAlignment@ is left inverse to
--   'asAlignment', that is,
--
--   > runAlignment . asAlignment === id
runAlignment :: (HasOrigin a, Boundable a) => Alignment (V a) -> a -> a
runAlignment (Alignment vs) = foldr (.) id $ map doAlign vs
  where doAlign v = alignBy v (magnitude v)