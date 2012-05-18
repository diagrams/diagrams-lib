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

import Prelude hiding (minimum)
import Data.Semigroup

import Data.AffineSpace
import Data.Monoid.PosInf (minimum)
import Data.VectorSpace

import Graphics.Rendering.Diagrams

import Diagrams.ThreeD.Types
import Diagrams.Solve

data Ellipsoid = Ellipsoid T3

type instance V Ellipsoid = R3

instance Transformable Ellipsoid where
  transform t1 (Ellipsoid t2) = Ellipsoid (t1 <> t2)

sphere :: (Backend b R3, Renderable Ellipsoid b) => Diagram b R3
sphere = mkQD (Prim $ Ellipsoid mempty)
              (mkEnvelope sphereEnv)
              (mkTrace sphereTrace)
              mempty
              (Query sphereQuery)
  where sphereEnv v = 1 / magnitude v
        sphereTrace p v = minimum (quadForm a b c)
          where a = v <.> v
                b = 2 *^ p' <.> v
                c = p' <.> p' - 1
                p' = p .-. origin
        sphereQuery v = Any $ magnitudeSq (v .-. origin) <= 1
