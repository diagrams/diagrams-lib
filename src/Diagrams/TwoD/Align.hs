{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

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
-- local origins relative to their envelopes or traces (or some other
-- sort of boundary).  For example, to align several diagrams along
-- their tops, we first move their local origins to the upper edge of
-- their boundary (using e.g. @map 'alignTop'@), and then put them
-- together with their local origins along a horizontal line (using
-- e.g. 'hcat' from "Diagrams.TwoD.Combinators").
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Align
    ( -- * Absolute alignment
      -- ** Align by envelope
      alignL, alignR, alignT, alignB
    , alignTL, alignTR, alignBL, alignBR

      -- ** Align by trace
    , snugL, snugR, snugT, snugB
    , snugTL, snugTR, snugBL, snugBR

      -- * Relative alignment
    , alignX, snugX, alignY, snugY

      -- * Centering
    , centerX, centerY, centerXY
    , snugCenterX, snugCenterY, snugCenterXY

    ) where

import           Diagrams.Core

import           Diagrams.Align
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector

-- | Align along the left edge, i.e. translate the diagram in a
--   horizontal direction so that the local origin is on the left edge
--   of the envelope.
alignL :: (InSpace V2 n a, Fractional n, Alignable a, HasOrigin a) => a -> a
alignL = align unit_X

snugL :: (InSpace V2 n a, Fractional n, Alignable a, Traced a, HasOrigin a) => a -> a
snugL = snug unit_X

-- | Align along the right edge.
alignR :: (InSpace V2 n a, Fractional n, Alignable a, HasOrigin a) => a -> a
alignR = align unitX

snugR :: (InSpace V2 n a, Fractional n, Alignable a, Traced a, HasOrigin a) => a -> a
snugR = snug unitX


-- | Align along the top edge.
alignT :: (InSpace V2 n a, Fractional n, Alignable a, HasOrigin a) => a -> a
alignT = align unitY

snugT:: (InSpace V2 n a, Fractional n, Alignable a, Traced a, HasOrigin a) => a -> a
snugT = snug unitY

-- | Align along the bottom edge.
alignB :: (InSpace V2 n a, Fractional n, Alignable a, HasOrigin a) => a -> a
alignB = align unit_Y

snugB :: (InSpace V2 n a, Fractional n, Alignable a, Traced a, HasOrigin a) => a -> a
snugB = snug unit_Y

alignTL, alignTR, alignBL, alignBR :: (InSpace V2 n a, Fractional n, Alignable a, HasOrigin a) => a -> a
alignTL = alignT . alignL
alignTR = alignT . alignR
alignBL = alignB . alignL
alignBR = alignB . alignR

snugTL, snugTR, snugBL, snugBR
  :: (InSpace V2 n a, Fractional n, Alignable a, Traced a, HasOrigin a)
   => a -> a
snugTL = snugT . snugL
snugTR = snugT . snugR
snugBL = snugB . snugL
snugBR = snugB . snugR

-- | @alignX@ and @snugX@ move the local origin horizontally as follows:
--
--   * @alignX (-1)@ moves the local origin to the left edge of the boundary;
--
--   * @align 1@ moves the local origin to the right edge;
--
--   * any other argument interpolates linearly between these.  For
--     example, @alignX 0@ centers, @alignX 2@ moves the origin one
--     \"radius\" to the right of the right edge, and so on.
--
--   * @snugX@ works the same way.

alignX :: (InSpace v n a, R1 v, Fractional n, Alignable a, HasOrigin a) => n -> a -> a
alignX = alignBy unitX

-- | See the documentation for 'alignX'.
snugX :: (InSpace v n a, R1 v, Fractional n, Alignable a, Traced a, HasOrigin a) => n -> a -> a
snugX = snugBy unitX

-- | Like 'alignX', but moving the local origin vertically, with an
--   argument of @1@ corresponding to the top edge and @(-1)@ corresponding
--   to the bottom edge.
alignY :: (InSpace v n a, R2 v, Fractional n, Alignable a, HasOrigin a) => n -> a -> a
alignY = alignBy unitY

-- | See the documentation for 'alignY'.
snugY :: (InSpace v n a, R2 v, Fractional n, Alignable a, Traced a, HasOrigin a) => n -> a -> a
snugY = snugBy unitY

-- | Center the local origin along the X-axis.
centerX :: (InSpace v n a, R1 v, Fractional n, Alignable a, HasOrigin a) => a -> a
centerX = alignBy unitX 0

snugCenterX :: (InSpace v n a, R1 v, Fractional n, Alignable a, Traced a, HasOrigin a) => a -> a
snugCenterX = snugBy unitX 0

-- | Center the local origin along the Y-axis.
centerY :: (InSpace v n a, R2 v, Fractional n, Alignable a, HasOrigin a) => a -> a
centerY = alignBy unitY 0

snugCenterY :: (InSpace v n a, R2 v, Fractional n, Alignable a, Traced a, HasOrigin a) => a -> a
snugCenterY = snugBy unitY 0

-- | Center along both the X- and Y-axes.
centerXY :: (InSpace v n a, R2 v, Fractional n, Alignable a, HasOrigin a) => a -> a
centerXY = centerX . centerY

snugCenterXY :: (InSpace v n a, R2 v, Fractional n, Alignable a, Traced a, HasOrigin a) => a -> a
snugCenterXY = snugCenterX . snugCenterY


