{-# LANGUAGE TypeSynonymInstances
           , TypeFamilies
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Types
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Basic types for three-dimensional Euclidean space.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Types
       ( -- * 3D Euclidean space
         R3
       , P3
       , T3

       ) where

import Graphics.Rendering.Diagrams

------------------------------------------------------------
-- 3D Euclidean space

-- | The three-dimensional Euclidean vector space R^3.
type R3 = (Double, Double, Double)

type instance V R3 = R3

-- | Points in R^3.
type P3 = Point R3

-- | Transformations in R^3.
type T3 = Transformation R3

instance Transformable R3 where
  transform = apply

