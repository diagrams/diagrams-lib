{-# LANGUAGE FlexibleContexts
  #-}

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

import Graphics.Rendering.Diagrams

import Control.Newtype

import Control.Arrow ((&&&))

import Data.VectorSpace

-- | The centroid of a set of /n/ points is their sum divided by /n/.
centroid :: (VectorSpace v, Fractional (Scalar v)) => [Point v] -> Point v
centroid = pack . uncurry (^/) . (sumV &&& (fromIntegral . length)) . map unpack
