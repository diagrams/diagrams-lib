{-# LANGUAGE TypeFamilies
           , FlexibleContexts
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
       ( align, alignBy
       , center
       ) where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Bounds

import Diagrams.Segment

import Data.VectorSpace
import Data.AffineSpace (alerp)

import Data.Monoid
import Data.Ratio

-- | @align v@ aligns a boundable object along the edge in the
--   direction of @v@.  That is, it moves the local origin in the
--   direction of @v@ until it is on the boundary.  (Note that if the
--   local origin is outside the boundary to begin, it may have to
--   move \"backwards\".)
align :: (HasOrigin a, Boundable a) => V a -> a -> a
align v a = moveOriginTo (boundary v a) a


-- XXX need a better, more intuitive description of alignBy

-- | @align v d a@ moves the origin of @a@ to a distance of @d*r@ from
--   the center along @v@, where @r@ is the radius along @v@.  Hence
--   @align v 0@ centers along @v@, and @align v 1@ moves the origin
--   in the direction of @v@ to the very edge of the bounding region.
alignBy :: (HasOrigin a, Boundable a) => V a -> Rational -> a -> a
alignBy v d a = moveOriginTo (alerp (boundary (negateV v) a)
                                    (boundary v a)
                                    ((fromRational d + 1) / 2))
                             a

-- | @center v@ centers a boundable object along the direction of @v@.
center :: (HasOrigin a, Boundable a) => V a -> a -> a
center v = alignBy v 0
