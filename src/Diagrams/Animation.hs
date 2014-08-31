{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
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
       , animEnvelope, animEnvelope'

       , animRect, animRect'

       ) where

import           Control.Applicative       ((<$>))
import           Data.Active
import           Data.Foldable             (foldMap)
import           Data.Semigroup

import           Diagrams.Core

import           Diagrams.Animation.Active ()
import           Diagrams.BoundingBox
import           Diagrams.Combinators
import           Diagrams.TrailLike
import           Diagrams.TwoD.Shapes
import           Diagrams.TwoD.Types


import           Linear.Metric

-- | A value of type @QAnimation b v m@ is an animation (a
--   time-varying diagram with start and end times) that can be
--   rendered by backspace @b@, with vector space @v@ and monoidal
--   annotations of type @m@.
type QAnimation b v n m = Active (QDiagram b v n m)

-- | A value of type @Animation b v@ is an animation (a time-varying
--   diagram with start and end times) in vector space @v@ that can be
--   rendered by backspace @b@.
--
--   Note that @Animation@ is actually a synonym for @QAnimation@
--   where the type of the monoidal annotations has been fixed to
--   'Any' (the default).
type Animation b v n = QAnimation b v n Any

-- $animComb
-- Most combinators for working with animations are to be found in the
-- @active@ package, which defines the 'Active' type.  This module
-- defines just a few combinators specifically for working with
-- animated diagrams.

-- It would be cool to have a variant of animEnvelope that tries to do
-- some sort of smart adaptive sampling to get good results more
-- quickly.  One could also imagine trying to use some sort of
-- automatic differentiation but that probably wouldn't work in all
-- cases we want to handle.

-- | Automatically assign fixed a envelope to the entirety of an
--   animation by sampling the envelope at a number of points in time
--   and taking the union of all the sampled envelopes to form the
--   \"hull\".  This hull is then used uniformly throughout the
--   animation.
--
--   This is useful when you have an animation that grows and shrinks
--   in size or shape over time, but you want it to take up a fixed
--   amount of space, /e.g./ so that the final rendered movie does not
--   zoom in and out, or so that it occupies a fixed location with
--   respect to another animation, when combining animations with
--   something like '|||'.
--
--   By default, 30 samples per time unit are used; to adjust this
--   number see 'animEnvelope''.
--
--   See also 'animRect' for help constructing a background to go
--   behind an animation.
animEnvelope :: (Backend b v n, OrderedField n, Metric v, Monoid' m)
           => QAnimation b v n m -> QAnimation b v n m
animEnvelope = animEnvelope' 30

-- | Like 'animEnvelope', but with an adjustible sample rate.  The first
--   parameter is the number of samples per time unit to use.  Lower
--   rates will be faster but less accurate; higher rates are more
--   accurate but slower.
animEnvelope' :: (Backend b v n, OrderedField n, Metric v, Monoid' m)
            => Rational -> QAnimation b v n m -> QAnimation b v n m
animEnvelope' r a = withEnvelope (simulate r a) <$> a

-- | @animRect@ works similarly to 'animEnvelope' for 2D diagrams, but
--   instead of adjusting the envelope, simply returns the smallest
--   bounding rectangle which encloses the entire animation.  Useful
--   for /e.g./ creating a background to go behind an animation.
--
--   Uses 30 samples per time unit by default; to adjust this number
--   see 'animRect''.
animRect :: (TrailLike t, Enveloped t, Transformable t, Monoid t, Vn t ~ V2 n, RealFloat n, Monoid' m)
         => QAnimation b V2 n m -> t
animRect = animRect' 30

-- | Like 'animRect', but with an adjustible sample rate.  The first
--   parameter is the number of samples per time unit to use.  Lower
--   rates will be faster but less accurate; higher rates are more
--   accurate but slower.
animRect' :: (TrailLike t, Enveloped t, Transformable t, Monoid t, Vn t ~ V2 n, RealFloat n, Monoid' m)
          => Rational -> QAnimation b V2 n m -> t
animRect' r anim
    | null results = rect 1 1
    | otherwise    = boxFit (foldMap boundingBox results) (rect 1 1)
  where
    results = simulate r anim
