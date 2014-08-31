{-# LANGUAGE TypeFamilies #-}

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

import           Linear.Vector

-- | Translate the diagram along unitX so that all points have
--   positive x-values.
alignXMin :: (Vn a ~ v n, Alignable a, HasOrigin a,
              R1 v, Additive v, Fractional n) => a -> a
alignXMin = align unit_X

snugXMin :: (Vn a ~ v n, Alignable a, Traced a, HasOrigin a,
             R1 v, Additive v, Fractional n) => a -> a
snugXMin = snug unit_X

-- | Translate the diagram along unitX so that all points have
-- negative x-values.
alignXMax :: (Vn a ~ v n, Alignable a, HasOrigin a,
              R1 v, Additive v, Fractional n) => a -> a
alignXMax = align unitX

snugXMax :: (Vn a ~ v n, Alignable a, Traced a, HasOrigin a,
             R1 v, Additive v, Fractional n) => a -> a
snugXMax = snug unitX


-- | Translate the diagram along unitY so that all points have
--   positive y-values.
alignYMin :: (Vn a ~ v n, Alignable a, HasOrigin a,
              R2 v, Additive v, Fractional n) => a -> a
alignYMin = align unit_Y

snugYMin :: (Vn a ~ v n, Alignable a, Traced a, HasOrigin a,
             R2 v, Additive v, Fractional n) => a -> a
snugYMin = snug unit_Y

-- | Translate the diagram along unitY so that all points have
-- negative y-values.
alignYMax :: (Vn a ~ v n, Alignable a, HasOrigin a,
              R2 v, Additive v, Fractional n) => a -> a
alignYMax = align unitY

snugYMax :: (Vn a ~ v n, Alignable a, Traced a, HasOrigin a,
             R2 v, Additive v, Fractional n) => a -> a
snugYMax = snug unitY


-- | Translate the diagram along unitZ so that all points have
--   positive z-values.
alignZMin :: (Vn a ~ v n, Alignable a, HasOrigin a,
              R3 v, Additive v, Fractional n) => a -> a
alignZMin = align unit_Z

snugZMin :: (Vn a ~ v n, Alignable a, Traced a, HasOrigin a,
             R3 v, Additive v, Fractional n) => a -> a
snugZMin = snug unit_Z

-- | Translate the diagram along unitZ so that all points have
-- negative z-values.
alignZMax :: (Vn a ~ v n, Alignable a, HasOrigin a,
              R3 v, Additive v, Fractional n) => a -> a
alignZMax = align unitZ

snugZMax :: (Vn a ~ v n, Alignable a, Traced a, HasOrigin a,
             R3 v, Additive v, Fractional n) => a -> a
snugZMax = snug unitZ

-- | Like 'alignX', but moving the local origin in the Z direction, with an
--   argument of @1@ corresponding to the top edge and @(-1)@ corresponding
--   to the bottom edge.
alignZ :: (Vn a ~ v n, Alignable a, HasOrigin a,
           R3 v, Additive v, Fractional n) => n -> a -> a
alignZ = alignBy unitZ

-- | See the documentation for 'alignZ'.
snugZ :: (Vn a ~ v n, Alignable a, Traced a, HasOrigin a,
          R3 v, Additive v, Fractional n) => n -> a -> a
snugZ = snugBy unitZ


-- | Center the local origin along the Z-axis.
centerZ :: (Vn a ~ v n, Alignable a, HasOrigin a,
            R3 v, Additive v, Fractional n) => a -> a
centerZ = alignBy unitZ 0

snugCenterZ :: (Vn a ~ v n, Alignable a, Traced a, HasOrigin a,
                R3 v, Additive v, Fractional n) => a -> a
snugCenterZ = snugBy unitZ 0

-- | Center along both the X- and Z-axes.
centerXZ :: (Vn a ~ v n, Alignable a, HasOrigin a,
             R3 v, Additive v, Fractional n) => a -> a
centerXZ = centerX . centerZ

snugCenterXZ :: (Vn a ~ v n, Alignable a, Traced a, HasOrigin a,
                 R3 v, Additive v, Fractional n) => a -> a
snugCenterXZ = snugCenterX . snugCenterZ


-- | Center along both the Y- and Z-axes.
centerYZ :: (Vn a ~ v n, Alignable a, HasOrigin a,
             R3 v, Additive v, Fractional n) => a -> a
centerYZ = centerZ . centerY

snugCenterYZ :: (Vn a ~ v n, Alignable a, Traced a, HasOrigin a,
                R3 v, Additive v, Fractional n) => a -> a
snugCenterYZ = snugCenterZ . snugCenterY

-- | Center an object in three dimensions.
centerXYZ :: (Vn a ~ v n, Alignable a, HasOrigin a,
             R3 v, Additive v, Fractional n) => a -> a
centerXYZ = centerX . centerY . centerZ

snugCenterXYZ :: (Vn a ~ v n, Alignable a, Traced a, HasOrigin a,
                 R3 v, Additive v, Fractional n) => a -> a
snugCenterXYZ = snugCenterX . snugCenterY . snugCenterZ

