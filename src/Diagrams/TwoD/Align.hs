{-# LANGUAGE FlexibleContexts
           , TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Align
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Alignment combinators in two dimensions.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Align
    (
      alignLeft, alignRight, alignTop, alignBottom
    , alignX, alignY
    , centerX, centerY, centerXY

    ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Util
import Diagrams.Align

import Data.VectorSpace

import Data.Monoid

-- | Align along the left edge.
alignLeft   :: (HasOrigin a, Boundable a, V a ~ R2) => a -> a
alignLeft   = align (negateV unitX)

-- | Align along the right edge.
alignRight  :: (HasOrigin a, Boundable a, V a ~ R2) => a -> a
alignRight  = align unitX

-- | Align along the top edge.
alignTop    :: (HasOrigin a, Boundable a, V a ~ R2) => a -> a
alignTop    = align unitY

-- | Align along the bottom edge.
alignBottom :: (HasOrigin a, Boundable a, V a ~ R2) => a -> a
alignBottom = align (negateV unitY)

-- | XXX comment me
alignX :: (HasOrigin a, Boundable a, V a ~ R2) => Rational -> a -> a
alignX = alignBy unitX

-- | XXX comment me
alignY :: (HasOrigin a, Boundable a, V a ~ R2) => Rational -> a -> a
alignY = alignBy unitY

-- | Center along the X-axis.
centerX  :: (HasOrigin a, Boundable a, V a ~ R2) => a -> a
centerX  = alignBy unitX 0

-- | Center along the Y-axis.
centerY  :: (HasOrigin a, Boundable a, V a ~ R2) => a -> a
centerY  = alignBy unitY 0

-- | Center along both the X- and Y-axes.
centerXY :: (HasOrigin a, Boundable a, V a ~ R2) => a -> a
centerXY = centerX . centerY
