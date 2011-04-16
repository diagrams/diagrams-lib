{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Animation
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools for creating animated diagrams.
--
-----------------------------------------------------------------------------

module Diagrams.Animation
  ( TimeDependentDiagram(..)
  , validateBounded
  , runAnimation
  , AnimationBackend(..)
  , staticDiagram
  , move
  , simultaneously
  ) where

import Data.AdditiveGroup
import Data.Monoid
import Data.VectorSpace

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Bounds (OrderedField)

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- a diagram that changes over time within the bounds of the interval
-- [startTime, endTime]
data (Fractional t) => TimeDependentDiagram b v a t = TimeDependentDiagram
  { diagramAtTime :: t -> AnnDiagram b v a
  , startTime :: t
  , endTime :: t
  }

validateBounded ::
 ( Backend b v
 , InnerSpace v
 , OrderedField t
 , OrderedField (Scalar v)
 , Monoid a
 ) => TimeDependentDiagram b v a t -> t -> AnnDiagram b v a
validateBounded tdd t = case withinBounds of
    True -> diagramAtTime tdd t
    False -> mempty
  where withinBounds = t >= startTime tdd && t <= endTime tdd

-- makes a list of diagrams to be treated as frames of animation
makeFrames :: (Fractional t) => Int -> TimeDependentDiagram b v a t
  -> [AnnDiagram b v a]
makeFrames numFrames tdd = let
    timeDelta = (endTime tdd - startTime tdd) / (fromIntegral numFrames)
    frameTimes = take numFrames $ iterate (+ timeDelta) (startTime tdd)
  in map (diagramAtTime tdd) frameTimes

-- for the moment, this will not be very general
-- we simply assume that animation is done in frames, and that
-- the length and speed of the animation is determined by the number of frames

class (Backend b v, Fractional t) => AnimationBackend b v t where
   type AnimatedRender b :: *
   renderAnim :: b -> [AnnDiagram b v a] -> AnimatedRender b

-- convenience function for running animation
runAnimation numFrames tdd animBackend =
   renderAnim animBackend $ makeFrames numFrames tdd

-- animation primitives ... kinda messy, not well-thought-out

--show an unchanging diagram for a length of time
staticDiagram :: (Fractional t) => AnnDiagram b v a -> t ->
  TimeDependentDiagram b v a t
staticDiagram diagram length = TimeDependentDiagram
  (const diagram) -- display this no matter what time
  0.0 -- start at time 0
  length

-- take a function of time-based translation, and create a TimeDependentDiagram
move diagram f length = TimeDependentDiagram
  (\t -> translate (f t) diagram)
  0.0
  length

-- runs animations in the union of their temporal spaces
simultaneously a b = TimeDependentDiagram
  (\t -> validateBounded a t <> validateBounded b t)
  (min (startTime a) (startTime b))
  (max (endTime a) (endTime b))


