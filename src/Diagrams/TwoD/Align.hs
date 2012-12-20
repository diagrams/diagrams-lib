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
-- local origins relative to their envelopes.  For example, to align
-- several diagrams along their tops, we first move their local
-- origins to the upper edge of their envelopes (using e.g. @map
-- 'alignTop'@), and then put them together with their local origins
-- along a horizontal line (using e.g. 'hcat' from
-- "Diagrams.TwoD.Combinators").
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Align
    ( -- * Absolute alignment
      alignL, alignR, alignT, alignB
    , alignTL, alignTR, alignBL, alignBR

      -- * Relative alignment
    , alignX, alignY

      -- * Centering
    , centerX, centerY, centerXY

    ) where

import Diagrams.Core

import Diagrams.TwoD.Types
import Diagrams.TwoD.Vector
import Diagrams.Align

import Data.VectorSpace

-- | Align along the left edge, i.e. translate the diagram in a
--   horizontal direction so that the local origin is on the left edge
--   of the envelope.
alignL :: ( Num a
          , AdditiveGroup a
          , Alignable b
          , V b ~ V2 a
          ) => b -> b
alignL = align (negateV unitX)

-- | Align along the right edge.
alignR :: ( Num a
          , Alignable b
          , V b ~ V2 a
          ) => b -> b
alignR = align unitX

-- | Align along the top edge.
alignT :: ( Num a
          , Alignable b
          , V b ~ V2 a
          ) => b -> b
alignT = align unitY

-- | Align along the bottom edge.
alignB :: ( Num a
          , AdditiveGroup a
          , Alignable b
          , V b ~ V2 a
          ) => b -> b
alignB = align (negateV unitY)

alignTL :: ( Num a
           , AdditiveGroup a
           , Alignable b
           , V b ~ V2 a
           ) => b -> b
alignTL = alignT . alignL

alignTR :: ( Num a
           , AdditiveGroup a
           , Alignable b
           , V b ~ V2 a
           ) => b -> b
alignTR = alignT . alignR

alignBL :: ( Num a
           , AdditiveGroup a
           , Alignable b
           , V b ~ V2 a
           ) => b -> b
alignBL = alignB . alignL

alignBR :: ( Num a
           , AdditiveGroup a
           , Alignable b
           , V b ~ V2 a
           ) => b -> b
alignBR = alignB . alignR

-- | @alignX@ moves the local origin horizontally as follows:
--
--   * @alignX (-1)@ moves the local origin to the left edge of the envelope;
--
--   * @align 1@ moves the local origin to the right edge;
--
--   * any other argument interpolates linearly between these.  For
--     example, @alignX 0@ centers, @alignX 2@ moves the origin one
--     \"radius\" to the right of the right edge, and so on.
alignX :: ( Num a
          , Alignable b
          , V b ~ V2 a
          ) => a -> b -> b
alignX = alignBy unitX

-- | Like 'alignX', but moving the local origin vertically, with an
--   argument of @1@ corresponding to the top edge and @(-1)@ corresponding
--   to the bottom edge.
alignY :: ( Num a
          , Alignable b
          , V b ~ V2 a
          ) => a -> b -> b
alignY = alignBy unitY

-- | Center the local origin along the X-axis.
centerX  :: ( Num a
            , Alignable b
            , V b ~ V2 a
            ) => b -> b
centerX  = alignBy unitX 0

-- | Center the local origin along the Y-axis.
centerY  :: ( Num a
            , Alignable b
            , V b ~ V2 a
            ) => b -> b
centerY  = alignBy unitY 0

-- | Center along both the X- and Y-axes.
centerXY :: ( Num a
            , Alignable b
            , V b ~ V2 a
            ) => b -> b
centerXY = centerX . centerY
