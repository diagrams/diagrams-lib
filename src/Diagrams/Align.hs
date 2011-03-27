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
-- General tools for alignment.
--
-----------------------------------------------------------------------------

module Diagrams.Align
       ( align, alignBy
       , center
       ) where

import Graphics.Rendering.Diagrams

import Data.VectorSpace

import Data.Ratio

-- | @align v@ moves a boundable object as far as possible in the
--   direction of @v@ until the origin is on the border of the
--   bounding region.  (Note, if the origin is outside the bounding
--   region to start, this may mean the object moves \"backwards\", in
--   the direction of @-v@.)
align :: ( HasOrigin a, Boundable a, v ~ BoundSpace a, v ~ OriginSpace a )
      => v -> a -> a
align v a = moveOriginBy (getBoundFunc (bounds a) v' *^ v') a
  where v' = negateV v


-- XXX need a better, more intuitive description of alignBy

-- | @align v d a@ moves the origin of @a@ to a distance of @d*r@ from
--   the center along @-v@, where @r@ is the radius along @v@.  Hence
--   @align v 0@ centers along @v@, and @align v 1@ moves the origin
--   along @-v@ to the very edge of the bounding region.  The negation
--   is because we actually want to think of @align@ as aligning the
--   /diagram/, not the origin.
alignBy :: ( HasOrigin a, Boundable a, v ~ BoundSpace a, v ~ OriginSpace a
           , s ~ Scalar v, Fractional s, AdditiveGroup s )
        => v -> Rational -> a -> a
alignBy v d a = moveOriginBy (v ^* (- radius v a * fromRational d)) a

-- | @center v@ centers a boundable object along the direction of @v@.
center :: ( HasOrigin a, Boundable a, v ~ BoundSpace a, v ~ OriginSpace a
          , s ~ Scalar v, Fractional s, AdditiveGroup s )
       => v -> a -> a
center v = alignBy v 0