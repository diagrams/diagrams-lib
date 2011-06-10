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
       ) where

import Graphics.Rendering.Diagrams

import Data.VectorSpace
import Data.AffineSpace (alerp)

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
