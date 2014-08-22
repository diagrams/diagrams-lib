{-# LANGUAGE TypeFamilies #-}
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
--   * A 'TrailLike' instance for @'Active' p@ where @p@ is also
--     'TrailLike', which simply lifts a pathlike thing to a constant
--     active value.
--
--   * A 'Juxtaposable' instance for @'Active' a@ where @a@ is also
--     'Juxtaposable'.  An active value can be juxtaposed against
--     another by doing the juxtaposition pointwise over time.  The
--     era of @juxtapose v a1 a2@ will be the same as the era of @a2@,
--     unless @a2@ is constant, in which case it will be the era of
--     @a1@.  (Note that @juxtapose v a1 a2@ and @liftA2 (juxtapose v)
--     a1 a2@ therefore have different semantics: the second is an
--     active value whose era is the /combination/ of the eras of @a1@
--     and @a2@).
--
--   * An 'Alignable' instance for @'Active' a@ where @a@ is also
--     'Alignable'; the active value is aligned pointwise over time.

-----------------------------------------------------------------------------

module Diagrams.Animation.Active where

import           Control.Applicative (pure, (<$>))

import           Diagrams.Core
import           Diagrams.TrailLike

import           Data.Active

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
--   juxtaposition pointwise over time.  The era of @juxtapose v a1
--   a2@ will be the same as the era of @a2@, unless @a2@ is constant,
--   in which case it will be the era of @a1@.  (Note that @juxtapose
--   v a1 a2@ and @liftA2 (juxtapose v) a1 a2@ therefore have
--   different semantics: the second is an active value whose era is
--   the /combination/ of the eras of @a1@ and @a2@).
instance Juxtaposable a => Juxtaposable (Active a) where

  juxtapose v a1 a2 =
    onActive       -- a1
      (\c1 ->        -- if a1 is constant, just juxtapose a2 pointwise with its value
        juxtapose v c1 <$> a2
      )
                     -- if a1 is dynamic...
      (onDynamic $ \s1 e1 d1 ->
        onActive      -- a2
          (\c2 ->      -- if a2 is constant, juxtapose pointwise with a1.  Since
                       --   the result will no longer be constant, the result
                       --   needs an era: we use a1's.
            mkActive s1 e1 (\t -> juxtapose v (d1 t) c2)
          )

                       -- otherwise, juxtapose pointwise, without changing a2's era
          (onDynamic $ \s2 e2 d2 ->
            mkActive s2 e2 (\t -> juxtapose v (d1 t) (d2 t))
          )
          a2
      )
      a1

--instance Alignable a => Alignable (Active a) where
--  alignBy v d a = alignBy v d <$> a
