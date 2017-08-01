{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Animation.Active
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- A few utilities and class instances for 'Active' (from the @active@
-- package).  In particular, this module defines
--
--   * An instance of 'V' for 'Active': @'V' ('Active' a) = 'V' a@
--
--   * 'HasOrigin', 'Transformable', and 'HasStyle' instances for
--     'Active' which all work pointwise.
--
--   * A 'TrailLike' instance for @'Active' t@ where @t@ is also
--     'TrailLike', which simply lifts a pathlike thing to an
--     (infinite) constant active value.
--
--   * 'Juxtaposable' instances for @'Active' a@ where @a@ is also
--     'Juxtaposable'.  An active value can be juxtaposed against
--     another by doing the juxtaposition pointwise over time.

-----------------------------------------------------------------------------

module Diagrams.Animation.Active where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative (liftA2, pure, (<$>))
#else
import           Control.Applicative (liftA2)
#endif

import           Diagrams.Core
import           Diagrams.TrailLike

import           Active

type instance V (Active a) = V a
type instance N (Active a) = N a

-- Yes, these are all orphan instances. Get over it.  We don't want to
-- put them in the 'active' package because 'active' is supposed to be
-- generally useful and shouldn't depend on diagrams.  We'd also
-- rather not put them in diagrams-core so that diagrams-core doesn't
-- have to depend on active.

instance HasOrigin a => HasOrigin (Active a) where
  moveOriginTo = fmap . moveOriginTo

instance Transformable a => Transformable (Active a) where
  transform = fmap . transform

instance HasStyle a => HasStyle (Active a) where
  applyStyle = fmap . applyStyle

instance TrailLike t => TrailLike (Active t) where
  trailLike = pure . trailLike

-- | An active value can be juxtaposed against another by doing the
-- juxtaposition pointwise over time.
instance Juxtaposable a => Juxtaposable (Active a) where
  juxtapose = liftA2 . juxtapose


-- XXX Alignable is kind of a mess

-- -- | An active value can be aligned by doing the alignment pointwise
-- --   over time.
-- instance (Alignable a, HasOrigin a) => Alignable (Active a) where
--   alignBy' b v d = fmap (alignBy' (\v a -> b v (  ) v d)
--   alignBy    v d = fmap (alignBy v d)

--   defaultBoundary v = defaultBoundary v . start
