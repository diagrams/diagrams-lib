{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
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
         Ellipsoid(..), sphere
       , Box(..), cube
       ) where

import           Data.Typeable
import           Control.Applicative
import           Control.Lens            ((^.), review)
import           Data.Semigroup

import           Data.AffineSpace
import           Data.Semigroup
import           Data.VectorSpace
import           Diagrams.Coordinates
import           Diagrams.Core
import           Diagrams.Solve
import           Diagrams.ThreeD.Types
import           Diagrams.ThreeD.Vector
import           Diagrams.TwoD.Types

data Ellipsoid = Ellipsoid T3
  deriving Typeable

type instance V Ellipsoid = R3

instance Transformable Ellipsoid where
  transform t1 (Ellipsoid t2) = Ellipsoid (t1 <> t2)

instance IsPrim Ellipsoid

instance Renderable Ellipsoid NullBackend where
  render _ _ = mempty

-- | A sphere of radius 1 with its center at the origin.
sphere :: (Backend b R3, Renderable Ellipsoid b) => Diagram b R3
sphere = mkQD (Prim $ Ellipsoid mempty)
              (mkEnvelope sphereEnv)
              (mkTrace sphereTrace)
              mempty
              (Query sphereQuery)
  where sphereEnv v = 1 / magnitude v
        sphereTrace p v = mkSortedList $ quadForm a b c
          where a = v <.> v
                b = 2 *^ p' <.> v
                c = p' <.> p' - 1
                p' = p .-. origin
        sphereQuery v = Any $ magnitudeSq (v .-. origin) <= 1

data Box = Box T3
         deriving (Typeable)

type instance V Box = R3

instance Transformable Box where
    transform t1 (Box t2) = Box (t1 <> t2)

instance IsPrim Box

instance Renderable Box NullBackend where
    render _ _ = mempty

-- | A cube with side length 1, in the positive octant, with one
-- vertex at the origin.
cube :: (Backend b R3, Renderable Box b) => Diagram b R3
cube = mkQD (Prim $ Box mempty)
            (mkEnvelope boxEnv)
            (mkTrace boxTrace)
            mempty
            (Query boxQuery)
  where
    corners = mkR3 <$> [0,1] <*> [0,1] <*> [0,1]
    boxEnv v = maximum (map (v <.>) corners) / magnitudeSq v
    -- ts gives all intersections with the planes forming the box
    -- filter keeps only those actually on the box surface
    boxTrace p v = mkSortedList . filter (range . atT) $ ts where
      (x0, y0, z0) = unp3 p
      (vx, vy, vz) = unr3 v
      intersections f d = case d of
          0 -> []
          _ -> [-f/d, (1-f)/d]
      ts = concat $ zipWith intersections [x0,y0,z0] [vx,vy,vz]
      atT t = p .+^ (t*^v)
    range u = and [x >= 0, x <= 1, y >= 0, y <= 1, z >= 0, z <= 1] where
      (x, y, z) = unp3 u
    boxQuery = Any . range
