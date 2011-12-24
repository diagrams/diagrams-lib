{-# LANGUAGE TypeFamilies
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Animation.Active
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- A few utilities and class instances for 'Active' (from the @active@
-- package).
-----------------------------------------------------------------------------

module Diagrams.Animation.Active where

import Graphics.Rendering.Diagrams
import Control.Applicative (pure)

import Diagrams.Path

import Data.Active

type instance V (Active a) = V a

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

instance PathLike p => PathLike (Active p) where
  pathLike st cl segs = pure (pathLike st cl segs)

{-
instance Juxtaposable a => Juxtaposable (Active a) where
  juxtapose v a1 a2 =
-}