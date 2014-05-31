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
       , pointDiagram
       , _pIso
       ) where

import           Diagrams.Core          (pointDiagram)
import           Diagrams.Core.Points

import           Control.Arrow          ((&&&))
import           Control.Lens           (Iso', iso)

import           Data.AffineSpace.Point
import           Data.VectorSpace

-- Point v <-> v
_pIso :: Iso' (Point v) v
_pIso = iso unPoint P

-- | The centroid of a set of /n/ points is their sum divided by /n/.
centroid :: (VectorSpace v, Fractional (Scalar v)) => [Point v] -> Point v
centroid = P . uncurry (^/) . (sumV &&& (fromIntegral . length)) . map unPoint
