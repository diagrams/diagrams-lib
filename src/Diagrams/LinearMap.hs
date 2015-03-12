{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.LinearMap
-- Copyright   :  (c) 2014 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Linear maps. Unlike 'Transformation's these are not restricted to the
-- same space. In practice these are used for projections in
-- "Diagrams.ThreeD.Projection".
--
-----------------------------------------------------------------------------

module Diagrams.LinearMap where

import           Control.Lens
import           Data.FingerTree         as FT
import qualified Data.Foldable           as F

import           Diagrams.Core
import           Diagrams.Core.Transform
import           Diagrams.Located
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Trail          hiding (offset)

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector


-- | Type for holding linear maps. Note that these are not affine transforms so
--   attemping apply a translation with 'LinearMap' will likely produce incorrect
--   results.
newtype LinearMap v u n = LinearMap { lapply :: v n -> u n }

toLinearMap :: Transformation v n -> LinearMap v v n
toLinearMap (Transformation (m :-: _) _ _) = LinearMap m

-- | Traversal over all the vmap of an object.
class LinearMappable a b where
  vmap :: (Vn a -> Vn b) -> a -> b
  -- this uses a function instead of LinearMap so we can also use this
  -- class to change number types

-- Note: instances need to be of the form
--
-- r ~ A u m => LinearMappable (A v n) r
--
-- so ghc knows there's only one possible result from calling vmap.

-- | Apply a linear map.
linmap :: (InSpace v n a, F.Foldable v, LinearMappable a b, N b ~ n)
     => LinearMap v (V b) n -> a -> b
linmap = vmap . lapply

instance r ~ Offset c u m => LinearMappable (Offset c v n) r where
  vmap f (OffsetClosed v) = OffsetClosed (f v)
  vmap _ OffsetOpen       = OffsetOpen
  {-# INLINE vmap #-}

instance r ~ Segment c u m => LinearMappable (Segment c v n) r where
  vmap f (Linear offset)      = Linear (vmap f offset)
  vmap f (Cubic v1 v2 offset) = Cubic (f v1) (f v2) (vmap f offset)
  {-# INLINE vmap #-}

instance (Metric v, Metric u, OrderedField n, OrderedField m, r ~ SegTree u m)
    => LinearMappable (SegTree v n) r where
  vmap f = over _Wrapped (fmap' (vmap f))
  {-# INLINE vmap #-}

instance (Metric v, Metric u, OrderedField n, OrderedField m, r ~ Trail' l u m)
    => LinearMappable (Trail' l v n) r where
  vmap f (Line st)        = Line (vmap f st)
  vmap f (Loop st offset) = Loop (vmap f st) (vmap f offset)
  {-# INLINE vmap #-}

instance (Metric v, Metric u, OrderedField n, OrderedField m, r ~ Trail u m)
    => LinearMappable (Trail v n) r where
  vmap f (Trail (Line st))        = Trail $ Line (vmap f st)
  vmap f (Trail (Loop st offset)) = Trail $ Loop (vmap f st) (vmap f offset)
  {-# INLINE vmap #-}

instance LinearMappable (Point v n) (Point u m) where
  vmap f (P v) = P (f v)
  {-# INLINE vmap #-}

instance r ~ FixedSegment u m => LinearMappable (FixedSegment v n) r where
  vmap f (FLinear p0 p1)      = FLinear (vmap f p0) (vmap f p1)
  vmap f (FCubic p0 p1 p2 p3) = FCubic (vmap f p0) (vmap f p1)
                                       (vmap f p2) (vmap f p3)
  {-# INLINE vmap #-}

instance (LinearMappable a b, r ~ Located b) => LinearMappable (Located a) r where
  vmap f (Loc p a) = Loc (vmap f p) (vmap f a)
  {-# INLINE vmap #-}

instance (Metric v, Metric u, OrderedField n, OrderedField m, r ~ Path u m)
    => LinearMappable (Path v n) r where
  vmap f = _Wrapped . mapped %~ vmap f
  {-# INLINE vmap #-}

-- | Affine linear maps. Unlike 'Transformation' these do not have to be
--   invertible so we can map between spaces.
data AffineMap v u n = AffineMap (LinearMap v u n) (u n)

-- | Make an affine map from a linear function and a translation.
mkAffineMap :: (v n -> u n) -> u n -> AffineMap v u n
mkAffineMap f = AffineMap (LinearMap f)

toAffineMap :: (HasBasis v, Num n)
            => Transformation v n -> AffineMap v v n
toAffineMap t = AffineMap (toLinearMap t) (transl t)

class (LinearMappable a b, N a ~ N b) => AffineMappable a b where
  amap :: (Additive (V a), F.Foldable (V a), Additive (V b), Num (N b))
       => AffineMap (V a) (V b) (N b) -> a -> b
  amap (AffineMap f _) = linmap f
  {-# INLINE amap #-}

instance r ~ Offset c u n => AffineMappable (Offset c v n) r
instance r ~ Segment c u n => AffineMappable (Segment c v n) r
instance (Metric v, Metric u, OrderedField n, r ~ SegTree u n) => AffineMappable (SegTree v n) r
instance (Metric v, Metric u, OrderedField n, r ~ Trail' l u n) => AffineMappable (Trail' l v n) r
instance (Metric v, Metric u, OrderedField n, r ~ Trail u n) => AffineMappable (Trail v n) r

instance (Additive v, F.Foldable v, Num n, r ~ Point u n) => AffineMappable (Point v n) r where
  amap (AffineMap f v) p = linmap f p .+^ v
  {-# INLINE amap #-}

instance r ~ FixedSegment u n => AffineMappable (FixedSegment v n) r where
  amap m (FLinear p0 p1)      = FLinear (amap m p0) (amap m p1)
  amap m (FCubic p0 p1 p2 p3) = FCubic (amap m p0) (amap m p1) (amap m p2) (amap m p3)
  {-# INLINE amap #-}

instance (LinearMappable a b, N a ~ N b, r ~ Located b) => AffineMappable (Located a) r where
  amap m@(AffineMap l _) (Loc p x) = Loc (amap m p) (linmap l x)
  {-# INLINE amap #-}

instance (Metric v, Metric u, OrderedField n, r ~ Path u n)
    => AffineMappable (Path v n) r where
  amap m = _Wrapped . mapped %~ amap m
  {-# INLINE amap #-}
