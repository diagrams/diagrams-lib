{-# LANGUAGE FlexibleContexts
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Combinators
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagram combinators in two dimensions.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Combinators
    (
      (===), (|||)
    ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Util
import Diagrams.Combinators

import Data.Monoid

-- | Place two diagrams (or other boundable objects) vertically
--   adjacent to one another.
(===) :: (HasOrigin a R2, Boundable a R2, Monoid a) => a -> a -> a
a1 === a2 = beside unitY a2 a1

-- | Place two diagrams (or other boundable objects) horizontally
--   adjacent to one another.
(|||) :: (HasOrigin a R2, Boundable a R2, Monoid a) => a -> a -> a
a1 ||| a2 = beside unitX a1 a2