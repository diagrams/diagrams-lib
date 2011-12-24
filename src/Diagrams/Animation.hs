{-# LANGUAGE TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Animation
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Animated diagrams.  Most of the tools for working with animations
-- can actually be found in "Data.Active".
--
-- XXX more documentation here
--
-----------------------------------------------------------------------------

module Diagrams.Animation where

import Graphics.Rendering.Diagrams

import Diagrams.Animation.Active ()

import Data.Active
import Data.Semigroup

-- | A value of type @QAnimation b v m@ is an animation that can be rendered by backspace @b@, with
type QAnimation b v m = Active (QDiagram b v m)
type Animation b v = QAnimation b v Any
