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

import           Diagrams.Core.Points

import           Control.Newtype

import           Control.Arrow        ((&&&))

import           Data.VectorSpace

-- | The centroid of a set of /n/ points is their sum divided by /n/.
centroid :: (VectorSpace v, Fractional (Scalar v)) => [Point v] -> Point v
centroid = pack . uncurry (^/) . (sumV &&& (fromIntegral . length)) . map unpack
