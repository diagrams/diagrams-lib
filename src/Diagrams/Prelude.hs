{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Prelude
-- Copyright   :  (c) 2011-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A module to re-export most of the functionality of the diagrams
-- core and standard library.
--
-----------------------------------------------------------------------------

module Diagrams.Prelude
  (
    -- * Diagrams library
    -- | Exports from this library for working with diagrams.
    module Diagrams

    -- * Convenience re-exports

    -- | For representing and operating on colors.
  , module Data.Colour

    -- | A large list of color names.
  , module Data.Colour.Names

    -- | Specify your own colours.
  , module Data.Colour.SRGB

    -- | Semigroups and monoids show up all over the place, so things from
    --   Data.Semigroup and Data.Monoid often come in handy.
  , module Data.Semigroup

    -- | For computing with vectors.
  , module Linear.Vector

    -- | For computing with points and vectors.
  , module Linear.Affine

    -- | For computing with dot products and norm.
  , module Linear.Metric

    -- | For working with 'Active' (i.e. animated) things.
  , module Data.Active

    -- | This exports most of the lens module, excluding the
    --   following functions that can cause collisions with diagrams:
    -- @contains@, @inside@, @outside@, @none@, @transform@, @(#)@
    -- (replaced by @##@) and @.>@ (which is just '.' anyway).
  , module Control.Lens

  , Applicative(..), (*>), (<*), (<$>), (<$), liftA, liftA2, liftA3
  ) where

import           Diagrams

import           Control.Applicative
import           Control.Lens               hiding (at, backwards, beside,
                                             contains, inside, none, outside,
                                             transform, ( # ), (.>))
import           Data.Active
import           Data.Colour                hiding (AffineSpace (..), atop, over)
import           Data.Colour.SRGB
import           Data.Colour.Names          hiding (tan)
import           Data.Semigroup

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

