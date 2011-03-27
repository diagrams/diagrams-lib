{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Types
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Basic types for two-dimensional Cartesian space.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Types
       ( R2
       , P2
       , Angle
       ) where

import Graphics.Rendering.Diagrams

-- | The two-dimensional Euclidean vector space R^2.
type R2 = (Double, Double)

-- | Points in R^2.
type P2 = Point R2

instance Transformable R2 where
  type TSpace R2 = R2
  transform = apply

-- | An angle in radians.
type Angle = Double
