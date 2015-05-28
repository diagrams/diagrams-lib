{-# LANGUAGE CPP                   #-}
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
       , Skinned(..)
       ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Lens              (review, (^.), _1)
import           Data.Typeable

import           Data.Semigroup
import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.Points
import           Diagrams.Solve.Polynomial
import           Diagrams.ThreeD.Types
import           Diagrams.ThreeD.Vector

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

instance OrderedField n => Enveloped (Ellipsoid n) where
    getEnvelope (Ellipsoid tr) = transform tr . mkEnvelope $ \v -> 1 / norm v

instance OrderedField n => Traced (Ellipsoid n) where
    getTrace (Ellipsoid tr) = transform tr . mkTrace $ \(P p) v -> let
        a  =      v `dot` v
        b  = 2 * (p `dot` v)
        c  =      p `dot` (p - 1)
        in
         mkSortedList $ quadForm a b c

-- | A sphere of radius 1 with its center at the origin.
sphere :: Num n => Ellipsoid n
sphere = Ellipsoid mempty

data Box n = Box (Transformation V3 n)
  deriving Typeable

type instance V (Box n) = V3
type instance N (Box n) = n

instance Fractional n => Transformable (Box n) where
    transform t1 (Box t2) = Box (t1 <> t2)

instance Fractional n => Renderable (Box n) NullBackend where
  render _ _ = mempty

instance OrderedField n => Enveloped (Box n) where
    getEnvelope (Box tr) = transform tr . mkEnvelope $ \v ->
        maximum (map (v `dot`) corners) / quadrance v where
          corners = mkR3 <$> [0,1] <*> [0,1] <*> [0,1]

instance (Fractional n, Ord n) => Traced (Box n) where
    getTrace (Box tr) = transform tr . mkTrace $ \p v -> let
        (x0, y0, z0) = unp3 p
        (vx, vy, vz) = unr3 v
        intersections f d = case d of
            0 -> []
            _ -> [-f/d, (1-f)/d]
        ts = concat $ zipWith intersections [x0,y0,z0] [vx,vy,vz]
        atT t = p .+^ (t*^v)
        range u = and [x >= 0, x <= 1, y >= 0, y <= 1, z >= 0, z <= 1] where
          (x, y, z) = unp3 u
        in
         -- ts gives all intersections with the planes forming the box
         -- filter keeps only those actually on the box surface
         mkSortedList . filter (range . atT) $ ts where

-- | A cube with side length 1, in the positive octant, with one
-- vertex at the origin.
cube :: Num n => Box n
cube = Box mempty

data Frustum n = Frustum n n (Transformation V3 n)
  deriving Typeable

type instance V (Frustum n) = V3
type instance N (Frustum n) = n

instance Fractional n => Transformable (Frustum n) where
  transform t1 (Frustum r0 r1 t2) = Frustum r0 r1 (t1 <> t2)

instance Fractional n => Renderable (Frustum n) NullBackend where
  render _ _ = mempty

instance (OrderedField n, RealFloat n) => Enveloped (Frustum n) where
    -- The plane containing v and the z axis intersects the frustum in a trapezoid
    -- Test the four corners of this trapezoid; one must determine the Envelope
    getEnvelope (Frustum r0 r1 tr) = transform tr . mkEnvelope $ \v ->let
        θ = v ^. _theta
        corners = [(r1,θ,1), (-r1,θ,1), (r0,θ,0), (-r0,θ,0)]
        in
         maximum . map (norm . project v . review r3CylindricalIso) $ corners

instance (RealFloat n, Ord n) => Traced (Frustum n) where
    -- The trace can intersect the sides of the cone or one of the end
    -- caps The sides are described by a quadric equation; substitute
    -- in the parametric form of the ray but disregard any
    -- intersections outside z = [0,1] Similarly, find intersections
    -- with the planes z=0, z=1, but disregard any r>r0, r>r1
    getTrace (Frustum r0 r1 tr) = transform tr . mkTrace $ \p v -> let
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
        in
         mkSortedList $ filter zbounds (quadForm a b c) ++ ends

-- | A frustum of a right circular cone.  It has height 1 oriented
-- along the positive z axis, and radii r0 and r1 at Z=0 and Z=1.
-- 'cone' and 'cylinder' are special cases.
frustum :: Num n => n -> n -> Frustum n
frustum r0 r1 = Frustum r0 r1 mempty

-- | A cone with its base centered on the origin, with radius 1 at the
-- base, height 1, and it's apex on the positive Z axis.
cone :: Num n => Frustum n
cone = frustum 1 0

-- | A circular cylinder of radius 1 with one end cap centered on the
-- origin, and extending to Z=1.
cylinder :: Num n => Frustum n
cylinder = frustum 1 1

-- | Types which can answer a Query about points inside the geometric object.
class Inside t where
    inside :: t -> Query (V t) (N t) Any

-- | Types which can be rendered as 3D Diagrams.
class Skinned t where
    skin :: (Renderable t b, N t ~ n, TypeableFloat n) => t  -> QDiagram b V3 n Any

instance (Num n, Ord n) => Inside (Ellipsoid n) where
    inside (Ellipsoid tr) = transform tr $
                            Query $ \v -> Any $ quadrance (v .-. origin) <= 1

instance OrderedField n => Skinned (Ellipsoid n) where
    skin s = mkQD (Prim s) (getEnvelope s) (getTrace s) mempty (inside s)

instance (Num n, Ord n) => Inside (Box n) where
    inside (Box tr) = transform tr . Query $ Any . range where
      range u = and [x >= 0, x <= 1, y >= 0, y <= 1, z >= 0, z <= 1] where
          (x, y, z) = unp3 u

instance OrderedField n => Skinned (Box n) where
    skin s = mkQD (Prim s) (getEnvelope s) (getTrace s) mempty (inside s)

instance (OrderedField n) => Inside (Frustum n) where
    inside (Frustum r0 r1 tr)= transform tr $
      Query $ \p -> let
          (x, _, z) = unp3 p
          r = r0 + (r1 - r0)*z
          v = p .-. origin
          a = norm $ projectXY v
          projectXY u = u ^-^ project unitZ u
          in
           Any $ x >= 0 && x <= 1 && a <= r

instance Skinned (Frustum n) where
    skin s = mkQD (Prim s) (getEnvelope s) (getTrace s) mempty (inside s)
