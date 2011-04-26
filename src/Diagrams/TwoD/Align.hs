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
-- Alignment combinators specialized for two dimensions.  See
-- "Diagrams.Align" for more general alignment combinators.
--
-- The basic idea is that alignment is achieved by moving diagrams'
-- local origins relative to their bounding regions.  For example, to
-- align several diagrams along their tops, we first move their local
-- origins to the upper edge of their bounding regions (using
-- e.g. @map 'alignTop'@), and then put them together with their local
-- origins along a horizontal line (using e.g. 'hcat' from
-- "Diagrams.TwoD.Combinators").
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Align
    ( -- * Absolute alignment
      alignLeft, alignRight, alignTop, alignBottom

      -- * Relative alignment
    , alignX, alignY

      -- * Centering
    , centerX, centerY, centerXY

    ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Util
import Diagrams.Align

import Data.VectorSpace

-- | Align along the left edge, i.e. move the local origin as far to
--   the left as possible.
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

-- | @alignX@ moves the local origin horizontally as follows:
--
--   * @alignX (-1)@ moves the local origin to the left edge of the bounding region;
--
--   * @align 1@ moves the local origin to the right edge;
--
--   * any other argument interpolates linearly between these.  For
--     example, @alignX 0@ centers, @alignX 2@ moves the origin one
--     \"radius\" to the right of the right edge, and so on.
alignX :: (HasOrigin a, Boundable a, V a ~ R2) => Rational -> a -> a
alignX = alignBy unitX

-- | Like 'alignX', but moving the local origin vertically, with an
--   argument of @1@ corresponding to the top edge and @(-1)@ corresponding
--   to the bottom edge.
alignY :: (HasOrigin a, Boundable a, V a ~ R2) => Rational -> a -> a
alignY = alignBy unitY

-- | Center the local origin along the X-axis.
centerX  :: (HasOrigin a, Boundable a, V a ~ R2) => a -> a
centerX  = alignBy unitX 0

-- | Center the local origin along the Y-axis.
centerY  :: (HasOrigin a, Boundable a, V a ~ R2) => a -> a
centerY  = alignBy unitY 0

-- | Center along both the X- and Y-axes.
centerXY :: (HasOrigin a, Boundable a, V a ~ R2) => a -> a
centerXY = centerX . centerY
