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

import Data.VectorSpace

import Data.Ratio

-- | @align v@ aligns a boundable object along the edge in the
--   direction of @v@.  That is, it moves the object as far as
--   possible in the opposite direction to @v@ until the origin is on
--   the border of the bounding region.  (Note, if the origin is
--   outside the bounding region to start, this may mean the object
--   moves \"backwards\", in the direction of @v@.)
align :: (HasOrigin a v, Boundable a v) => v -> a -> a
align v a = moveOriginBy (getBoundFunc (bounds a) v *^ v) a
  -- XXX create a function boundary and use it here


-- XXX need a better, more intuitive description of alignBy

-- | @align v d a@ moves the origin of @a@ to a distance of @d*r@ from
--   the center along @v@, where @r@ is the radius along @v@.  Hence
--   @align v 0@ centers along @v@, and @align v 1@ moves the origin
--   in the direction of @v@ to the very edge of the bounding region.
alignBy :: (HasOrigin a v, Boundable a v) => v -> Rational -> a -> a
alignBy v d a = moveOriginBy (v ^* (- radius v a * fromRational d)) a

-- | @center v@ centers a boundable object along the direction of @v@.
center :: (HasOrigin a v, Boundable a v) => v -> a -> a
center v = alignBy v 0