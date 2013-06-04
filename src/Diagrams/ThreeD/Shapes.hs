{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , FlexibleInstances
           , InstanceSigs
           , MultiParamTypeClasses
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
       , sphere,
         NurbsSurface(..),
         surfaceGrid
       ) where

import Prelude hiding (minimum)
import Data.Semigroup
import qualified Data.Vector as V
import Control.Arrow

import Data.AffineSpace
import Data.Monoid.PosInf (minimum)
import Data.VectorSpace
import Math.Spline.Knots

import Diagrams.Core

import Diagrams.ThreeD.NurbsSurface
import Diagrams.ThreeD.Types
import Diagrams.Solve

data Ellipsoid = Ellipsoid T3

type instance V Ellipsoid = R3

instance Transformable Ellipsoid where
  transform t1 (Ellipsoid t2) = Ellipsoid (t1 <> t2)

instance IsPrim Ellipsoid

instance Renderable Ellipsoid NullBackend where
  render _ _ = mempty

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

type instance V NurbsSurface = R3

instance Transformable NurbsSurface where
  transform t (NurbsSurface uk vk cps) = NurbsSurface uk vk cps' where
    cps' = (map . map) tr cps
    tr (H w r) = H w (transform t r)

instance IsPrim NurbsSurface

instance Renderable NurbsSurface NullBackend where
  render _ _ = mempty

