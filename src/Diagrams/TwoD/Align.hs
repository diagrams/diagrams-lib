{-# LANGUAGE FlexibleContexts
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

    , strutX, strutY

    ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Util
import Diagrams.Align

import Data.VectorSpace

import Data.Monoid

-- | Align along the left edge.
alignLeft   :: (HasOrigin a R2, Boundable a R2) => a -> a
alignLeft   = align (negateV unitX)

-- | Align along the right edge.
alignRight  :: (HasOrigin a R2, Boundable a R2) => a -> a
alignRight  = align unitX

-- | Align along the top edge.
alignTop    :: (HasOrigin a R2, Boundable a R2) => a -> a
alignTop    = align unitY

-- | Align along the bottom edge.
alignBottom :: (HasOrigin a R2, Boundable a R2) => a -> a
alignBottom = align (negateV unitY)

-- | XXX comment me
alignX :: (HasOrigin a R2, Boundable a R2) => Rational -> a -> a
alignX = alignBy unitX

-- | XXX comment me
alignY :: (HasOrigin a R2, Boundable a R2) => Rational -> a -> a
alignY = alignBy unitY

-- | Center along the X-axis.
centerX  :: (HasOrigin a R2, Boundable a R2) => a -> a
centerX  = alignBy unitX 0

-- | Center along the Y-axis.
centerY  :: (HasOrigin a R2, Boundable a R2) => a -> a
centerY  = alignBy unitY 0

-- | Center along both the X- and Y-axes.
centerXY :: (HasOrigin a R2, Boundable a R2) => a -> a
centerXY = centerX . centerY

-- | @strutX d@ is an empty diagram with width @d@ and height 0.
strutX :: (Backend b R2, Monoid m) => Double -> AnnDiagram b R2 m
strutX d = strut (d,0)

-- | @strutY d@ is an empty diagram with height @d@ and width 0.
strutY :: (Backend b R2, Monoid m) => Double -> AnnDiagram b R2 m
strutY d = strut (0,d)