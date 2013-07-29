{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Points
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Some miscellaneous utilities for working with points.
--
-----------------------------------------------------------------------------

module Diagrams.Points
       ( centroid

       ) where

import           Diagrams.Coordinates
import           Diagrams.Core.Points

import           Control.Newtype

import           Control.Arrow        ((&&&))

import           Data.VectorSpace

-- | The centroid of a set of /n/ points is their sum divided by /n/.
centroid :: (VectorSpace v, Fractional (Scalar v)) => [Point v] -> Point v
centroid = pack . uncurry (^/) . (sumV &&& (fromIntegral . length)) . map unpack

instance Coordinates v => Coordinates (Point v) where
  type FinalCoord (Point v)    = FinalCoord v
  type PrevDim (Point v)       = PrevDim v
  type Decomposition (Point v) = Decomposition v

  x & y        = P (x & y)
  coords (P v) = coords v
