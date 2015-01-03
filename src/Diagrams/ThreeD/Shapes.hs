{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
       , Frustum(..) , frustum, cone, cylinder
       ) where

import           Control.Applicative
import           Control.Lens           (review, (^.), _1)
import           Data.Typeable

import           Data.Semigroup
import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.Solve
import           Diagrams.ThreeD.Types
import           Diagrams.ThreeD.Vector
import           Diagrams.Points

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

data Ellipsoid n = Ellipsoid (Transformation V3 n)
  deriving Typeable

type instance V (Ellipsoid n) = V3
type instance N (Ellipsoid n) = n

instance Fractional n => Transformable (Ellipsoid n) where
  transform t1 (Ellipsoid t2) = Ellipsoid (t1 <> t2)

instance Fractional n => Renderable (Ellipsoid n) NullBackend where
  render _ _ = mempty

-- | A sphere of radius 1 with its center at the origin.
sphere :: (Typeable n, OrderedField n, Renderable (Ellipsoid n) b) => QDiagram b V3 n Any
sphere = mkQD (Prim $ Ellipsoid mempty)
              (mkEnvelope sphereEnv)
              (mkTrace sphereTrace)
              mempty
              (Query sphereQuery)
  where
    sphereEnv v         = 1 / norm v
    sphereTrace (P p) v = mkSortedList $ quadForm a b c
      where
        a  =      v `dot` v
        b  = 2 * (p `dot` v)
        c  =      p `dot` (p - 1)
    sphereQuery v = Any $ quadrance (v .-. origin) <= 1

data Box n = Box (Transformation V3 n)
  deriving Typeable

type instance V (Box n) = V3
type instance N (Box n) = n

instance Fractional n => Transformable (Box n) where
    transform t1 (Box t2) = Box (t1 <> t2)

instance Fractional n => Renderable (Box n) NullBackend where
  render _ _ = mempty

-- | A cube with side length 1, in the positive octant, with one
-- vertex at the origin.
cube :: (Typeable n, OrderedField n, Renderable (Box n) b) => QDiagram b V3 n Any
cube = mkQD (Prim $ Box mempty)
            (mkEnvelope boxEnv)
            (mkTrace boxTrace)
            mempty
            (Query boxQuery)
  where
    corners = mkR3 <$> [0,1] <*> [0,1] <*> [0,1]
    boxEnv v = maximum (map (v `dot`) corners) / quadrance v
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

data Frustum n = Frustum n n (Transformation V3 n)
  deriving Typeable

type instance V (Frustum n) = V3
type instance N (Frustum n) = n

instance Fractional n => Transformable (Frustum n) where
  transform t1 (Frustum r0 r1 t2) = Frustum r0 r1 (t1 <> t2)

instance Fractional n => Renderable (Frustum n) NullBackend where
  render _ _ = mempty

-- | A frustum of a right circular cone.  It has height 1 oriented
-- along the positive z axis, and radii r0 and r1 at Z=0 and Z=1.
-- 'cone' and 'cylinder' are special cases.
frustum :: (TypeableFloat n, Renderable (Frustum n) b) => n -> n -> QDiagram b V3 n Any
frustum r0 r1 = mkQD (Prim $ Frustum r0 r1 mempty)
                 (mkEnvelope frEnv)
                 (mkTrace frTrace)
                 mempty
                 (Query frQuery)
  where
    projectXY u = u ^-^ project unitZ u
    frQuery p = Any $ x >= 0 && x <= 1 && a <= r where
      (x, _, z) = unp3 p
      r = r0 + (r1 - r0)*z
      v = p .-. origin
      a = norm $ projectXY v
    -- The plane containing v and the z axis intersects the frustum in a trapezoid
    -- Test the four corners of this trapezoid; one must determine the Envelope
    frEnv v = maximum . map (norm . project v . review r3CylindricalIso) $ corners
      where
        θ = v ^. _theta
        corners = [(r1,θ,1), (-r1,θ,1), (r0,θ,0), (-r0,θ,0)]
    -- The trace can intersect the sides of the cone or one of the end
    -- caps The sides are described by a quadric equation; substitute
    -- in the parametric form of the ray but disregard any
    -- intersections outside z = [0,1] Similarly, find intersections
    -- with the planes z=0, z=1, but disregard any r>r0, r>r1
    frTrace p v = mkSortedList $ filter zbounds (quadForm a b c) ++ ends
      where
        (px, py, pz) = unp3 p
        (vx, vy, vz) = unr3 v
        ray t = p .+^ t *^ v
        dr = r1 - r0
        a = vx**2 + vy**2 - vz**2 * dr**2
        b = 2 * (px * vx + py * vy - (r0+pz*dr) * dr  * vz)
        c = px**2 + py**2 - (r0 + dr*pz)**2
        zbounds t = ray t ^. _z >= 0
                 && ray t ^. _z <= 1
        ends = concatMap cap [0,1]
        cap z = [ t | ray t ^. lensP . r3CylindricalIso . _1 < r0 + z * dr ]
          where
            t = (z - pz) / vz

-- | A cone with its base centered on the origin, with radius 1 at the
-- base, height 1, and it's apex on the positive Z axis.
cone :: (TypeableFloat n, Renderable (Frustum n) b) => QDiagram b V3 n Any
cone = frustum 1 0

-- | A circular cylinder of radius 1 with one end cap centered on the
-- origin, and extending to Z=1.
cylinder :: (TypeableFloat n, Renderable (Frustum n) b) => QDiagram b V3 n Any
cylinder = frustum 1 1
