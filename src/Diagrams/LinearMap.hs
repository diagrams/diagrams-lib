{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
module Diagrams.LinearMap where

import           Control.Lens        hiding (lmap)
import           Data.FingerTree     as FT

import           Diagrams.Core
import           Diagrams.Core.Transform
import           Diagrams.Located
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Trail      hiding (offset)

import           Linear.Affine
import           Linear.Metric

import Linear.Vector


-- | Type for holding linear maps. Note that these are not affine transforms so
--   attemping apply a translation with 'LinearMap' will likely produce incorrect
--   results.
newtype LinearMap v u n = LinearMap { lapply :: v n -> u n }

toLinearMap :: Transformation v n -> LinearMap v v n
toLinearMap (Transformation (m :-: _) _ _) = LinearMap m

-- vmap :: (InSpace v n a, InSpace u n b, LinearMappable a b) => LinearMap v u n -> a -> b
-- vmap (LinearMap m) = over vmap m

-- | Traversal over all the vmap of an object.
class LinearMappable a b where
  vmap :: (Vn a -> Vn b) -> a -> b
  -- this uses a function instead of LinearMap so we can also use this class to
  -- change number types

-- | Apply a linear map.
lmap :: (LinearMappable a b, N a ~ N b) => LinearMap (V a) (V b) (N a) -> a -> b
lmap = vmap . lapply

instance LinearMappable (Offset c v n) (Offset c u m) where
  vmap f (OffsetClosed v) = OffsetClosed (f v)
  vmap _ OffsetOpen       = OffsetOpen
  {-# INLINE vmap #-}

instance LinearMappable (Segment c v n) (Segment c u m) where
  vmap f (Linear offset)      = Linear (vmap f offset)
  vmap f (Cubic v1 v2 offset) = Cubic (f v1) (f v2) (vmap f offset)
  {-# INLINE vmap #-}

instance (Metric v, Metric u, OrderedField n, OrderedField m)
    => LinearMappable (SegTree v n) (SegTree u m) where
  vmap f = over _Wrapped (fmap' (vmap f))
  {-# INLINE vmap #-}

instance (Metric v, Metric u, OrderedField n, OrderedField m)
    => LinearMappable (Trail' l v n) (Trail' l u m) where
  vmap f (Line st)        = Line (vmap f st)
  vmap f (Loop st offset) = Loop (vmap f st) (vmap f offset)
  {-# INLINE vmap #-}

instance (Metric v, Metric u, OrderedField n, OrderedField m)
    => LinearMappable (Trail v n) (Trail u m) where
  vmap f (Trail (Line st))        = Trail $ Line (vmap f st)
  vmap f (Trail (Loop st offset)) = Trail $ Loop (vmap f st) (vmap f offset)
  {-# INLINE vmap #-}

instance LinearMappable (Point v n) (Point u m) where
  vmap f (P v) = P  (f v)
  {-# INLINE vmap #-}

instance LinearMappable (FixedSegment v n) (FixedSegment u m) where
  vmap f (FLinear p0 p1)      = FLinear (vmap f p0) (vmap f p1)
  vmap f (FCubic p0 p1 p2 p3) = FCubic (vmap f p0) (vmap f p1)
                                       (vmap f p2) (vmap f p3)
  {-# INLINE vmap #-}

instance LinearMappable a b => LinearMappable (Located a) (Located b) where
  vmap f (Loc p a) = Loc (vmap f p) (vmap f a)
  {-# INLINE vmap #-}

instance (Metric v, Metric u, OrderedField n, OrderedField m)
    => LinearMappable (Path v n) (Path u m) where
  vmap f = over (_Wrapped . mapped) (vmap f)
  {-# INLINE vmap #-}

-- | Affine linear maps. Unlike Transformation these do not have to be
--   invertable so we can map between spaces.
data AffineMap v u n = AffineMap (LinearMap v u n) (u n)

toAffineMap :: Transformation v n -> AffineMap v v n
toAffineMap (Transformation (m :-: _) _ v) = AffineMap (LinearMap m) v

class (LinearMappable a b, N a ~ N b) => AffineMappable a b where
  amap :: (Additive (V b), Num (N b)) => AffineMap (V a) (V b) (N b) -> a -> b
  amap (AffineMap f _) = lmap f
  {-# INLINE amap #-}

instance AffineMappable (Offset c v n) (Offset c u n)
instance AffineMappable (Segment c v n) (Segment c u n)
instance (Metric v, Metric u, OrderedField n) => AffineMappable (SegTree v n) (SegTree u n)
instance (Metric v, Metric u, OrderedField n) => AffineMappable (Trail' l v n) (Trail' l u n)
instance (Metric v, Metric u, OrderedField n) => AffineMappable (Trail v n) (Trail u n)

instance AffineMappable (Point v n) (Point u n) where
  amap (AffineMap f v) (P p) = P (lapply f p ^+^ v)
  {-# INLINE amap #-}

instance AffineMappable (FixedSegment v n) (FixedSegment u n) where
  amap m (FLinear p0 p1)      = FLinear (amap m p0) (amap m p1)
  amap m (FCubic p0 p1 p2 p3) = FCubic (amap m p0) (amap m p1) (amap m p2) (amap m p3)
  {-# INLINE amap #-}

instance AffineMappable a b => AffineMappable (Located a) (Located b) where
  amap m (Loc p x) = Loc (amap m p) (amap m x)
  {-# INLINE amap #-}

instance (Metric v, Metric u, OrderedField n)
    => AffineMappable (Path v n) (Path u n) where
  amap m = _Wrapped . mapped %~ amap m
  {-# INLINE amap #-}

