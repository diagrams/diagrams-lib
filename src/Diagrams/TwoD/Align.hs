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
    , centerX, centerY, centerXY

    , strutX, strutY

    ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.Align

import Data.Monoid

-- | Align along the left edge.
alignLeft   :: (HasOrigin a R2, Boundable a R2) => a -> a
alignLeft   = align (-1,0)

-- | Align along the right edge.
alignRight  :: (HasOrigin a R2, Boundable a R2) => a -> a
alignRight  = align (1,0)

-- | Align along the top edge.
alignTop    :: (HasOrigin a R2, Boundable a R2) => a -> a
alignTop    = align (0,1)

-- | Align along the bottom edge.
alignBottom :: (HasOrigin a R2, Boundable a R2) => a -> a
alignBottom = align (0,-1)

-- | Center along the X-axis.
centerX  :: (HasOrigin a R2, Boundable a R2) => a -> a
centerX  = alignBy (1,0) 0

-- | Center along the Y-axis.
centerY  :: (HasOrigin a R2, Boundable a R2) => a -> a
centerY  = alignBy (0,1) 0

-- | Center along both the X- and Y-axes.
centerXY :: (HasOrigin a R2, Boundable a R2) => a -> a
centerXY = centerX . centerY

-- | @strutX d@ is an empty diagram with width @d@ and height 0.
strutX :: (Backend b R2, Monoid m) => Double -> AnnDiagram b R2 m
strutX d = strut (d,0)

-- | @strutY d@ is an empty diagram with height @d@ and width 0.
strutY :: (Backend b R2, Monoid m) => Double -> AnnDiagram b R2 m
strutY d = strut (0,d)