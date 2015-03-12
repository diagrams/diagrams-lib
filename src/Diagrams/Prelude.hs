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

    -- | For working with default values. Diagrams also exports 'with',
    --   an alias for 'def'.
  , module Data.Default.Class

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

    -- | Most of the lens package. The following functions are not
    --   exported from lens because they either conflict with
    --   diagrams or may conflict with other libraries:
    --
    --   * 'Control.Lens....'
    --   * 'Control.Lens..>'
    --   * 'Control.Lens.<.>'
    --   * 'Control.Lens.argument'
    --   * 'Control.Lens.at'
    --   * 'Control.Lens.beside'
    --   * 'Control.Lens.children'
    --   * 'Control.Lens.coerce'
    --   * 'Control.Lens.contains'
    --   * 'Control.Lens.index'
    --   * 'Control.Lens.indexed'
    --   * 'Control.Lens.indices'
    --   * 'Control.Lens.inside'
    --   * 'Control.Lens.levels'
    --   * 'Control.Lens.none'
    --   * 'Control.Lens.outside'
    --   * 'Control.Lens.singular'
    --   * 'Control.Lens.transform'
  , module Control.Lens

  , Applicative(..), (*>), (<*), (<$>), (<$), liftA, liftA2, liftA3
  ) where

import           Diagrams

import           Control.Applicative
import           Control.Lens               hiding (argument, at, backwards,
                                             beside, children, coerce, contains,
                                             indexed, indices, inside, levels,
                                             none, outside, singular, transform,
                                             ( # ), (...), (.>), (<.>))
import           Data.Active
import           Data.Default.Class
import           Data.Colour                hiding (AffineSpace (..), atop,
                                             over)
import           Data.Colour.Names          hiding (tan)
import           Data.Colour.SRGB
import           Data.Semigroup

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

