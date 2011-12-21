{-# LANGUAGE TypeFamilies
  #-}
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

module Diagrams.Animation where

import Graphics.Rendering.Diagrams

import Diagrams.Animation.Active

import Data.Active
import Data.Semigroup

type QAnimation b v m = Active (QDiagram b v m)
type Animation b v = QAnimation b v Any
