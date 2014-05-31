{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleContexts #-}

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

import           Data.VectorSpace

-- | Align along the left edge, i.e. translate the diagram in a
--   horizontal direction so that the local origin is on the left edge
--   of the envelope.
alignL :: (Alignable a, HasOrigin a, V a ~ v, R2Ish v) => a -> a
alignL = align (negateV unitX)

snugL :: (Fractional (Scalar (V a)), Alignable a, Traced a,
      HasOrigin a, V a ~ v, R2Ish v) => a -> a
snugL = snug (negateV unitX)

-- | Align along the right edge.
alignR :: (Alignable a, HasOrigin a, V a ~ v, R2Ish v) => a -> a
alignR = align unitX

snugR :: (Fractional (Scalar (V a)), Alignable a, Traced a,
      HasOrigin a, V a ~ v, R2Ish v) => a -> a
snugR = snug unitX


-- | Align along the top edge.
alignT :: (Alignable a, HasOrigin a, V a ~ v, R2Ish v) => a -> a
alignT = align unitY

snugT:: (Fractional (Scalar (V a)), Alignable a, Traced a,
      HasOrigin a, V a ~ v, R2Ish v) => a -> a
snugT = snug unitY

-- | Align along the bottom edge.
alignB :: (Alignable a, HasOrigin a, V a ~ v, R2Ish v) => a -> a
alignB = align (negateV unitY)

snugB :: (Fractional (Scalar (V a)), Alignable a, Traced a,
      HasOrigin a, V a ~ v, R2Ish v) => a -> a
snugB = snug (negateV unitY)

alignTL, alignTR, alignBL, alignBR :: (Alignable a, HasOrigin a, V a ~ v, R2Ish v) => a -> a
alignTL = alignT . alignL
alignTR = alignT . alignR
alignBL = alignB . alignL
alignBR = alignB . alignR

snugTL, snugTR, snugBL, snugBR
  :: (Fractional (Scalar (V a)), Alignable a, Traced a, HasOrigin a, V a ~ v, R2Ish v)
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

alignX :: (Alignable a, HasOrigin a, V a ~ v, R2Ish v) => Scalar v -> a -> a
alignX = alignBy unitX

-- | See the documentation for 'alignX'.
snugX :: (Fractional (Scalar (V a)), Alignable a, Traced a,
      HasOrigin a, V a ~ v, R2Ish v) => Scalar v -> a -> a
snugX = snugBy unitX

-- | Like 'alignX', but moving the local origin vertically, with an
--   argument of @1@ corresponding to the top edge and @(-1)@ corresponding
--   to the bottom edge.
alignY :: (Alignable a, HasOrigin a, V a ~ v, R2Ish v) => Scalar v -> a -> a
alignY = alignBy unitY

snugY :: (Fractional (Scalar (V a)), Alignable a, Traced a,
      HasOrigin a, V a ~ v, R2Ish v) => Scalar v -> a -> a
snugY = snugBy unitY

-- | Center the local origin along the X-axis.
centerX  :: (Alignable a, HasOrigin a, V a ~ v, R2Ish v) => a -> a
centerX  = alignBy unitX 0

snugCenterX :: (Fractional (Scalar (V a)), Alignable a, Traced a,
      HasOrigin a, V a ~ v, R2Ish v) => a -> a
snugCenterX = snugBy unitX 0

-- | Center the local origin along the Y-axis.
centerY  :: (Alignable a, HasOrigin a, V a ~ v, R2Ish v) => a -> a
centerY  = alignBy unitY 0

snugCenterY :: (Fractional (Scalar (V a)), Alignable a, Traced a,
      HasOrigin a, V a ~ v, R2Ish v) => a -> a
snugCenterY = snugBy unitY 0

-- | Center along both the X- and Y-axes.
centerXY :: (Alignable a, HasOrigin a, V a ~ v, R2Ish v) => a -> a
centerXY = center

snugCenterXY :: (Fractional (Scalar (V a)), Alignable a, Traced a,
      HasOrigin a, V a ~ v, R2Ish v) => a -> a
snugCenterXY = snugCenter
