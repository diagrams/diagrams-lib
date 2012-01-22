{-# LANGUAGE TypeFamilies
           , FlexibleContexts
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Animation
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- An animation is a time-varying diagram, together with start and end
-- times.  Most of the tools for working with animations can actually
-- be found in the @active@ package, which defines the 'Active' type.
--
-- XXX more documentation and examples should go here
--
-----------------------------------------------------------------------------

module Diagrams.Animation
       ( -- * Types for animations
         QAnimation
       , Animation

         -- * Animation combinators and tools
         -- $animComb
       , autoBounds, autoBounds'

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Combinators
import Diagrams.Animation.Active ()

import Data.Active
import Data.Semigroup

import Data.VectorSpace

import Control.Applicative ((<$>))

-- | A value of type @QAnimation b v m@ is an animation (a
--   time-varying diagram with start and end times) that can be
--   rendered by backspace @b@, with vector space @v@ and monoidal
--   annotations of type @m@.
type QAnimation b v m = Active (QDiagram b v m)

-- | A value of type @Animation b v@ is an animation (a time-varying
--   diagram with start and end times) in vector space @v@ that can be
--   rendered by backspace @b@.
--
--   Note that @Animation@ is actually a synonym for @QAnimation@
--   where the type of the monoidal annotations has been fixed to
--   'Any' (the default).
type Animation b v = QAnimation b v Any

-- $animComb
-- Most combinators for working with animations are to be found in the
-- @active@ package, which defines the 'Active' type.  This module
-- defines just a few combinators specifically for working with
-- animated diagrams.

-- It would be cool to have a variant of autoBounds that tries to do
-- some sort of smart adaptive sampling to get good results more
-- quickly.  One could also imagine trying to use some sort of
-- automatic differentiation but that probably wouldn't work in all
-- cases we want to handle.

-- | Automatically assign fixed bounds to an animation, by sampling
--   the bounds at a number of points in time and taking the union of
--   all the sampled bounds to form the \"hull\".  This hull is then
--   used uniformly throughout the animation.
--
--   This is useful when you have an animation that grows and shrinks
--   in size or shape over time, but you want it to take up a fixed
--   amount of space, /e.g./ so that the final rendered movie does not
--   zoom in and out.
--
--   By default, does 30 samples per time unit; to adjust this number
--   see 'autoBounds''.
autoBounds :: (Backend b v, OrderedField (Scalar v), InnerSpace v, Monoid' m)
           => QAnimation b v m -> QAnimation b v m
autoBounds = autoBounds' 30

-- | Like 'autoBounds', but with an adjustible sample rate.  The first
--   parameter is the number of samples per time unit to use.  Lower
--   rates will be faster but less accurate; higher rates are more
--   accurate but slower.
autoBounds' :: (Backend b v, OrderedField (Scalar v), InnerSpace v, Monoid' m)
            => Rational -> QAnimation b v m -> QAnimation b v m
autoBounds' r a = withBounds (simulate r a) <$> a