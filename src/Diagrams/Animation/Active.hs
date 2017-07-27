{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
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
--   * An instance of 'V' for 'Active': @'V' ('Active' d f a) = 'V' a@
--
--   * 'HasOrigin', 'Transformable', and 'HasStyle' instances for
--     'Active' which all work pointwise.
--
--   * A 'TrailLike' instance for @'Active' d I t@ where @t@ is also
--     'TrailLike', which simply lifts a pathlike thing to an
--     (infinite) constant active value.
--
--   * 'Juxtaposable' instances for @'Active' d F a@ and @'Active' d I
--     a@ where @a@ is also 'Juxtaposable'.  An active value can be
--     juxtaposed against another by doing the juxtaposition pointwise
--     over time.  The era of @juxtapose v a1 a2@ will be the same as
--     the era of @a2@, unless @a2@ is constant, in which case it will
--     be the era of @a1@.  (Note that @juxtapose v a1 a2@ and @liftA2
--     (juxtapose v) a1 a2@ therefore have different semantics: the
--     second is an active value whose era is the /combination/ of the
--     eras of @a1@ and @a2@).
--
--   * An 'Alignable' instance for @'Active' a@ where @a@ is also
--     'Alignable'; the active value is aligned pointwise over time.

-----------------------------------------------------------------------------

module Diagrams.Animation.Active where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative  (pure, (<$>))
#endif

import           Diagrams.Align
import           Diagrams.Core
import           Diagrams.TrailLike

import           Active
import           Control.IApplicative

type instance V (Active f a) = V a
type instance N (Active f a) = N a

-- Yes, these are all orphan instances. Get over it.  We don't want to
-- put them in the 'active' package because 'active' is supposed to be
-- generally useful and shouldn't depend on diagrams.  We'd also
-- rather not put them in diagrams-core so that diagrams-core doesn't
-- have to depend on active.

instance HasOrigin a => HasOrigin (Active f a) where
  moveOriginTo = fmap . moveOriginTo

instance Transformable a => Transformable (Active f a) where
  transform = fmap . transform

instance HasStyle a => HasStyle (Active f a) where
  applyStyle = fmap . applyStyle

instance TrailLike t => TrailLike (Active 'I t) where
  trailLike = ipure . trailLike

-- | A finite active value can be juxtaposed against another by doing
--   the juxtaposition pointwise over time.  The duration of the result
--   is the minimum of the two durations.
--
--   Note this is just @'iliftA2' . 'juxtapose'@, with a more
--   restricted type.  In particular, you can use @'iliftA2'
--   . 'juxtapose'@ directly to juxtapose two 'Active' values where
--   one is finite and the other infinite.
instance Juxtaposable a => Juxtaposable (Active 'F a) where

  juxtapose = iliftA2 . juxtapose

-- | An infinite active value can be juxtaposed against another by
--   doing the juxtaposition pointwise over time.
--
--   Note this is just @'iliftA2' . 'juxtapose'@, with a more
--   restricted type.  In particular, you can use @'iliftA2'
--   . 'juxtapose'@ directly to juxtapose two 'Active' values where
--   one is finite and the other infinite.
instance Juxtaposable a => Juxtaposable (Active 'I a) where
  juxtapose = iliftA2 . juxtapose


-- XXX Alignable is kind of a mess

-- -- | An active value can be aligned by doing the alignment pointwise
-- --   over time.
-- instance (Alignable a, HasOrigin a) => Alignable (Active f a) where
--   alignBy' b v d = fmap (alignBy' (\v a -> b v (  ) v d)
--   alignBy    v d = fmap (alignBy v d)

--   defaultBoundary v = defaultBoundary v . start
