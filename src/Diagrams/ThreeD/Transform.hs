{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Transform
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Transformations specific to three dimensions, with a few generic
-- transformations (uniform scaling, translation) also re-exported for
-- convenience.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Transform where

import           Diagrams.Core
import qualified Diagrams.Core.Transform as T

import           Control.Newtype         (over)

import           Diagrams.Coordinates
import           Diagrams.Transform
import           Diagrams.ThreeD.Types
import           Diagrams.ThreeD.Vector    (direction)

import           Data.Semigroup

import           Data.AffineSpace

import           Control.Arrow           (first, second)

-- | Create a transformation which rotates by the given angle about
--   a line parallel the Z axis passing through the local origin.
--   A positive angle brings positive x-values towards the positive-y axis.
--
--   The angle can be expressed using any type which is an
--   instance of 'Angle'.  For example, @rotate (1\/4 ::
--   'CircleFrac')@, @rotate (tau\/4 :: 'Rad')@, and @rotate (90 ::
--   'Deg')@ all represent the same transformation, namely, a
--   counterclockwise rotation by a right angle.  To rotate about some
--   point other than the local origin, see 'rotateAbout'.
--
--   Note that writing @rotate (1\/4)@, with no type annotation, will
--   yield an error since GHC cannot figure out which sort of angle
--   you want to use.  In this common situation you can use
--   'rotateBy', which is specialized to take a 'CircleFrac' argument.
aboutZ :: (Tranformable t, V t ~ R3, Angle a) => a -> t -> t
aboutZ = undefined