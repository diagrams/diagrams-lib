{-# LANGUAGE TypeFamilies    #-}

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
import           Diagrams.TwoD.Align

-- | Translate the diagram along unitX so that all points have
--   positive x-values.
alignXMin :: (InSpace v n a, R1 v, Fractional n, Alignable a, HasOrigin a) => a -> a
alignXMin = align unit_X

snugXMin :: (InSpace v n a, R1 v, Fractional n, Alignable a, HasOrigin a, Traced a) => a -> a
snugXMin = snug unit_X

-- | Translate the diagram along unitX so that all points have
-- negative x-values.
alignXMax :: (InSpace v n a, R1 v, Fractional n, Alignable a, HasOrigin a) => a -> a
alignXMax = align unitX

snugXMax :: (InSpace v n a, R1 v, Fractional n, Alignable a, HasOrigin a, Traced a) => a -> a
snugXMax = snug unitX


-- | Translate the diagram along unitY so that all points have
--   positive y-values.
alignYMin :: (InSpace v n a, R2 v, Fractional n, Alignable a, HasOrigin a) => a -> a
alignYMin = align unit_Y

snugYMin :: (InSpace v n a, R2 v, Fractional n, Alignable a, HasOrigin a, Traced a) => a -> a
snugYMin = snug unit_Y

-- | Translate the diagram along unitY so that all points have
-- negative y-values.
alignYMax :: (InSpace v n a, R2 v, Fractional n, Alignable a, HasOrigin a) => a -> a
alignYMax = align unitY

snugYMax :: (InSpace v n a, R2 v, Fractional n, Alignable a, HasOrigin a, Traced a) => a -> a
snugYMax = snug unitY


-- | Translate the diagram along unitZ so that all points have
--   positive z-values.
alignZMin :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a) => a -> a
alignZMin = align unit_Z

snugZMin :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a, Traced a) => a -> a
snugZMin = snug unit_Z

-- | Translate the diagram along unitZ so that all points have
-- negative z-values.
alignZMax :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a) => a -> a
alignZMax = align unitZ

snugZMax :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a, Traced a) => a -> a
snugZMax = snug unitZ

-- | Like 'alignX', but moving the local origin in the Z direction, with an
--   argument of @1@ corresponding to the top edge and @(-1)@ corresponding
--   to the bottom edge.
alignZ :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a) => n -> a -> a
alignZ = alignBy unitZ

-- | See the documentation for 'alignZ'.
snugZ :: (V a ~ v, N a ~ n, Alignable a, Traced a, HasOrigin a,
          R3 v, Fractional n) => n -> a -> a
snugZ = snugBy unitZ


-- | Center the local origin along the Z-axis.
centerZ :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a) => a -> a
centerZ = alignBy unitZ 0

snugCenterZ :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a, Traced a) => a -> a
snugCenterZ = snugBy unitZ 0

-- | Center along both the X- and Z-axes.
centerXZ :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a) => a -> a
centerXZ = centerX . centerZ

snugCenterXZ :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a, Traced a) => a -> a
snugCenterXZ = snugCenterX . snugCenterZ


-- | Center along both the Y- and Z-axes.
centerYZ :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a) => a -> a
centerYZ = centerZ . centerY

snugCenterYZ :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a, Traced a) => a -> a
snugCenterYZ = snugCenterZ . snugCenterY

-- | Center an object in three dimensions.
centerXYZ :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a) => a -> a
centerXYZ = centerX . centerY . centerZ

snugCenterXYZ :: (InSpace v n a, R3 v, Fractional n, Alignable a, HasOrigin a, Traced a) => a -> a
snugCenterXYZ = snugCenterX . snugCenterY . snugCenterZ
