{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , ViewPatterns
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Shapes
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Various three-dimensional shapes.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Shapes
       (
         Ellipsoid(..)
       , sphere
       ) where

import Graphics.Rendering.Diagrams

import Diagrams.ThreeD.Types

import Data.Semigroup

data Ellipsoid = Ellipsoid T3

type instance V Ellipsoid = R3

instance Transformable Ellipsoid where
  transform t1 (Ellipsoid t2) = Ellipsoid (t1 <> t2)

sphere :: (Backend b R3, Renderable Ellipsoid b) => Diagram b R3
sphere = mkQD (Prim $ Ellipsoid mempty)
              (mkEnvelope sphereEnv)
              mempty
              (Query sphereQuery)
  where sphereEnv (unr3 -> (x,y,z))   = 1 / sqrt(x*x + y*y + z*z)
        sphereQuery (unp3 -> (x,y,z)) = Any $ x*x + y*y + z*z <= 1
