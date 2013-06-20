{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , FlexibleInstances
           , InstanceSigs
           , MultiParamTypeClasses
           , ViewPatterns
           , GADTs
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
         cylinder,
         cyl,
         NurbsSurface(..),
       ) where

import Prelude hiding (minimum)
import Data.Semigroup
import Data.Monoid.PosInf
import qualified Data.Vector as V
import Control.Arrow

import Data.AffineSpace
import Data.Monoid.PosInf (minimum)
import Data.VectorSpace
import Math.Spline.Knots as K

import Diagrams.Core

import Math.NurbsSurface
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

type instance V (NurbsSurface Double R3) = R3

instance Transformable (NurbsSurface Double R3) where
  transform t (NurbsSurface uk vk cps) = NurbsSurface uk vk cps' where
    cps' = (map.map) (second (transform t)) cps

instance IsPrim (NurbsSurface Double R3)

instance Renderable (NurbsSurface Double R3) NullBackend where
  render _ _ = mempty

cylinder :: (Backend b R3, Renderable (NurbsSurface Double R3) b) => Diagram b R3
cylinder = nurbsQD $ cyl

-- for debugging purposes, seperate this from cylinder above
cyl :: NurbsSurface Double R3
cyl = NurbsSurface
           (K.mkKnots [0,0,1,1])
           (K.mkKnots [0,0,0,0.25,0.25,0.5,0.5,0.75,0.75,1,1,1])
           [zipWith toH wts circ, zipWith toH wts $ map (^+^ xhat) circ] where
             xhat = r3 (1,0,0)
             wts = concat. repeat $ [1, sqrt 2 / 2]
             circ = map r3 [(0,1,0),   (0,1,1),
                            (0,0,1),   (0,-1,1),
                            (0,-1,0), (0,-1,-1),
                            (0,0,-1), (0,1,-1),
                            (0,1,0)]

nurbsQD :: (Backend b R3, Renderable (NurbsSurface Double R3) b) =>
           NurbsSurface Double R3 -> Diagram b R3
nurbsQD n = mkQD (Prim n) nurbsEnv nurbsTrace mempty nurbsQuery where
  -- XXX TODO these are placeholders, entirely incorrect
  nurbsEnv = undefined
  nurbsTrace = undefined
  nurbsQuery = undefined
