{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Align
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Alignment combinators specialized for three dimensions.  See
-- "Diagrams.Align" for more general alignment combinators.
--
-- The basic idea is that alignment is achieved by moving diagrams'
-- local origins relative to their envelopes or traces (or some other
-- sort of boundary).  For example, to align several diagrams along
-- their tops, we first move their local origins to the upper edge of
-- their boundary (using e.g. @map 'alignZMax'@), and then put them
-- together with their local origins along a line (using e.g. 'cat'
-- from "Diagrams.Combinators").
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Align
    ( -- * Absolute alignment
      -- ** Align by envelope
      alignXMin, alignXMax, alignYMin, alignYMax, alignZMin, alignZMax

      -- ** Align by trace
    , snugXMin, snugXMax, snugYMin, snugYMax, snugZMin, snugZMax

      -- * Relative alignment
    , alignX, snugX, alignY, snugY, alignZ, snugZ

      -- * Centering
    , centerX, centerY, centerZ
    , centerXY, centerXZ, centerYZ, centerXYZ
    , snugCenterX, snugCenterY, snugCenterZ
    , snugCenterXY, snugCenterXZ, snugCenterYZ, snugCenterXYZ

    ) where

import           Diagrams.Core

import           Diagrams.Align
import           Diagrams.ThreeD.Types
import           Diagrams.ThreeD.Vector

import Linear.Vector

-- | Translate the diagram along unitX so that all points have
-- positive x-values.
alignXMin :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
alignXMin = align (negated unitX)

snugXMin :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugXMin = snug (negated unitX)

-- | Translate the diagram along unitX so that all points have
-- negative x-values.
alignXMax :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
alignXMax = align unitX

snugXMax :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugXMax = snug unitX


-- | Translate the diagram along unitY so that all points have
-- negative y-values.
alignYMax :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
alignYMax = align unitY

-- | Move the origin along unitY until it touches the edge of the
-- diagram.
snugYMax:: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugYMax = snug unitY

-- | Translate the diagram along unitY so that all points have
-- positive y-values.
alignYMin :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
alignYMin = align (negated unitY)

snugYMin :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugYMin = snug (negated unitY)

-- | Translate the diagram along unitZ so that all points have
-- negative z-values.
alignZMax :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
alignZMax = align unitZ

-- | Move the origin along unitZ until it touches the edge of the
-- diagram.
snugZMax:: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugZMax = snug unitZ

-- | Translate the diagram along unitZ so that all points have
-- positive z-values.
alignZMin :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
alignZMin = align (negated unitZ)

-- | Move the origin along unit_Z until it touches the edge of the
-- diagram.
snugZMin :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugZMin = snug (negated unitZ)

-- | @alignX@ and @snugX@ move the local origin along unitX as follows:
--
--   * @alignX (-1)@ moves the local origin to the low-x of the boundary;
--
--   * @align 1@ moves the local origin to the high-x edge;
--
--   * any other argument interpolates linearly between these.  For
--     example, @alignX 0@ centers, @alignX 2@ moves the origin one
--     \"radius\" to the right of the right edge, and so on.
--
--   * @snugX@ works the same way.

alignX :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => n -> a -> a
alignX = alignBy unitX

-- | See the documentation for 'alignX'.
snugX :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => n -> a -> a
snugX = snugBy unitX

-- | Like 'alignX', but moving the local origin vertically, with an
--   argument of @1@ corresponding to the top edge and @(-1)@ corresponding
--   to the bottom edge.
alignY :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => n -> a -> a
alignY = alignBy unitY

snugY :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => n -> a -> a
snugY = snugBy unitY


-- | Like 'alignX', but moving the local origin in the Z direction, with an
--   argument of @1@ corresponding to the top edge and @(-1)@ corresponding
--   to the bottom edge.
alignZ :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => n -> a -> a
alignZ = alignBy unitZ

snugZ :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => n -> a -> a
snugZ = snugBy unitZ

-- | Center the local origin along the X-axis.
centerX  :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
centerX  = alignBy unitX 0

snugCenterX :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugCenterX = snugBy unitX 0

-- | Center the local origin along the Y-axis.
centerY  :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
centerY  = alignBy unitY 0

snugCenterY :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugCenterY = snugBy unitY 0

-- | Center the local origin along the Z-axis.
centerZ  :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
centerZ  = alignBy unitZ 0

snugCenterZ :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugCenterZ = snugBy unitZ 0

-- | Center along both the X- and Y-axes.
centerXY :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
centerXY = centerX . centerY

snugCenterXY :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugCenterXY = snugCenterX . snugCenterY


-- | Center along both the X- and Z-axes.
centerXZ :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
centerXZ = centerX . centerZ

snugCenterXZ :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugCenterXZ = snugCenterX . snugCenterZ


-- | Center along both the Y- and Z-axes.
centerYZ :: (Alignable a, HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
centerYZ = centerZ . centerY

snugCenterYZ :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugCenterYZ = snugCenterZ . snugCenterY

-- | Center an object in three dimensions.
centerXYZ :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
centerXYZ = centerX . centerY . centerZ

snugCenterXYZ :: (Fractional n, Alignable a, Traced a,
      HasOrigin a, Vn a ~ V3 n, Floating n) => a -> a
snugCenterXYZ = snugCenterX . snugCenterY . snugCenterZ

