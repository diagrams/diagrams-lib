{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Points
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Points in space.  For more tools for working with points and
-- vectors, see "Data.AffineSpace" and "Diagrams.Coordinates".
--
-----------------------------------------------------------------------------

module Diagrams.Points
       ( -- * Points
         Point, origin, (*.)

         -- * Point-related utilities
       , centroid

       ) where

import           Diagrams.Coordinates
import           Diagrams.Core.Points

import           Control.Arrow          ((&&&))

import           Data.AffineSpace.Point
import           Data.VectorSpace

-- | The centroid of a set of /n/ points is their sum divided by /n/.
centroid :: (VectorSpace v, Fractional (Scalar v)) => [Point v] -> Point v
centroid = P . uncurry (^/) . (sumV &&& (fromIntegral . length)) . map unPoint

instance Coordinates v => Coordinates (Point v) where
  type FinalCoord (Point v)    = FinalCoord v
  type PrevDim (Point v)       = PrevDim v
  type Decomposition (Point v) = Decomposition v

  x & y        = P (x & y)
  coords (P v) = coords v
