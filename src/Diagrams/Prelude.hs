-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Prelude
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A module to re-export most of the functionality of the diagrams
-- core and standard library.
--
-----------------------------------------------------------------------------

module Diagrams.Prelude
       (
         module Graphics.Rendering.Diagrams

       , module Diagrams.Attributes
       , module Diagrams.Combinators
       , module Diagrams.Path
       , module Diagrams.Segment
       , module Diagrams.TwoD
       , module Diagrams.Util

       , module Data.Monoid

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Attributes
import Diagrams.Combinators
import Diagrams.Path
import Diagrams.Segment
import Diagrams.TwoD
import Diagrams.Util

import Data.Default

import Data.Monoid


