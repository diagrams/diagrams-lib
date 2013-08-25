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
-- local origins relative to their boundries (e.g. envelope or trace).
-- For example, to align several diagrams along their tops, we first move
-- their local origins to the upper edge of their boundries (using e.g. @map
-- 'alignTop'@), and then put them together with their local origins
-- along a horizontal line (using e.g. 'hcat' from
-- "Diagrams.TwoD.Combinators").
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Align
    ( -- * Absolute alignment
      alignL, alignR, alignT, alignB
    , alignTL, alignTR, alignBL, alignBR

    , alignL', alignR', alignT', alignB'
    , alignTL', alignTR', alignBL', alignBR'

      -- * Relative alignment
    , alignX, alignY
    , alignX', alignY'

      -- * Centering
    , centerX, centerY, centerXY
    , centerX', centerY', centerXY'

    , AlignOpts(..)
    , traceOriginP

    ) where

import Diagrams.Core

import Diagrams.TwoD.Types
import Diagrams.TwoD.Vector
import Diagrams.Align

import Data.VectorSpace

-- | Align along the left edge of the boundry as determined by the boundry
--   function in opts.
alignL' :: (Alignable a, V a ~ R2) => AlignOpts -> a -> a
alignL' opts = align' opts (negateV unitX)

-- | Align along the left edge, i.e. translate the diagram in a
--   horizontal direction so that the local origin is on the left edge
--   of the boundry.
alignL :: (Alignable a, V a ~ R2) => a -> a
alignL = align (negateV unitX)

-- | Align along the right edge.
alignR':: (Alignable a, V a ~ R2) => AlignOpts -> a -> a
alignR' opts = align' opts unitX

alignR :: (Alignable a, V a ~ R2) => a -> a
alignR = align unitX

-- | Align along the top edge.
alignT' :: (Alignable a, V a ~ R2) => AlignOpts -> a -> a
alignT' opts = align' opts unitY

alignT :: (Alignable a, V a ~ R2) => a -> a
alignT = align unitY

-- | Align along the bottom edge.
alignB' :: (Alignable a, V a ~ R2) => AlignOpts -> a -> a
alignB' opts = align' opts (negateV unitY)

alignB :: (Alignable a, V a ~ R2) => a -> a
alignB = align (negateV unitY)

alignTL', alignTR', alignBL', alignBR'
  :: (Alignable a, V a ~ R2) => AlignOpts ->a -> a
alignTL' opts = alignT' opts . alignL' opts
alignTR' opts = alignT' opts . alignR' opts
alignBL' opts = alignB' opts . alignL' opts
alignBR' opts = alignB' opts . alignR' opts

alignTL, alignTR, alignBL, alignBR :: (Alignable a, V a ~ R2) => a -> a
alignTL = alignT . alignL
alignTR = alignT . alignR
alignBL = alignB . alignL
alignBR = alignB . alignR

-- | @alignX@  and @alignX'@ move the local origin horizontally as follows:
--
--   * @alignX (-1)@ moves the local origin to the left edge of the boundry;
--
--   * @align 1@ moves the local origin to the right edge;
--
--   * any other argument interpolates linearly between these.  For
--     example, @alignX 0@ centers, @alignX 2@ moves the origin one
--     \"radius\" to the right of the right edge, and so on.
alignX' :: (Alignable a, V a ~ R2) => AlignOpts -> Double -> a -> a
alignX' opts = alignBy' opts unitX

alignX :: (Alignable a, V a ~ R2) => Double -> a -> a
alignX = alignBy unitX

-- | Like 'alignX', but moving the local origin vertically, with an
--   argument of @1@ corresponding to the top edge and @(-1)@ corresponding
--   to the bottom edge.
alignY' :: (Alignable a, V a ~ R2) => AlignOpts -> Double -> a -> a
alignY' opts = alignBy' opts unitY

alignY :: (Alignable a, V a ~ R2) => Double -> a -> a
alignY = alignBy unitY

-- | Center the local origin along the X-axis.
centerX'  :: (Alignable a, V a ~ R2) => AlignOpts -> a -> a
centerX' opts  = alignBy' opts unitX 0

centerX  :: (Alignable a, V a ~ R2) => a -> a
centerX  = alignBy unitX 0

-- | Center the local origin along the Y-axis.
centerY'  :: (Alignable a, V a ~ R2) => AlignOpts -> a -> a
centerY' opts = alignBy' opts unitY 0

centerY  :: (Alignable a, V a ~ R2) => a -> a
centerY  = alignBy unitY 0

-- | Center along both the X- and Y-axes.
centerXY' :: (Alignable a, V a ~ R2) => AlignOpts -> a -> a
centerXY' opts = centerX' opts . centerY' opts

centerXY :: (Alignable a, V a ~ R2) => a -> a
centerXY = centerX . centerY
