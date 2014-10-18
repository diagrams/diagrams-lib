{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
module Diagrams.LinearMap where

import           Control.Applicative
import           Control.Lens        hiding (pre)
import           Data.FingerTree     as FT
import           Data.Typeable
-- import           GHC.Float

import           Diagrams.Core
import           Diagrams.Core.Transform
import           Diagrams.Located
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Trail      hiding (offset)

import           Linear.Affine
import           Linear.Metric

import GHC.Exts (Constraint)
import Linear.Vector

-- -- | Traversal over all the vectors of an object.
-- class HasVectors a b where
--   vectors :: Traversal a b (Vn a) (Vn b)
--   -- it's temping to make this overloaded, i.e.
--     -- HasVectors a v u n n' where vectors :: Traversal (a v n) (a u n') (v n) (u n')
-- 
-- instance HasVectors (Offset c v n) (Offset c u n') where
--   vectors f (OffsetClosed v) = OffsetClosed <$> f v
--   vectors _ OffsetOpen       = pure OffsetOpen
--   {-# INLINE vectors #-}
-- 
-- instance HasVectors (Segment c v n) (Segment c u n') where
--   vectors f (Linear offset)      = Linear <$> vectors f offset
--   vectors f (Cubic v1 v2 offset) = Cubic <$> f v1 <*> f v2 <*> vectors f offset
--   {-# INLINE vectors #-}
-- 
-- instance (Metric v, OrderedField n, Metric u, OrderedField n')
--     => HasVectors (SegTree v n) (SegTree u n') where
--   vectors = _Wrapped . traverse' . vectors
--   {-# INLINE vectors #-}
-- 
-- instance (Metric v, OrderedField n, Metric u, OrderedField n')
--     => HasVectors (Trail' l v n) (Trail' l u n') where
--   vectors f (Line st)        = Line <$> vectors f st
--   vectors f (Loop st offset) = Loop <$> vectors f st <*> vectors f offset
--   {-# INLINE vectors #-}
-- 
-- instance (Metric v, OrderedField n, Metric u, OrderedField n')
--     => HasVectors (Trail v n) (Trail u n') where
--   vectors f (Trail (Line st))        = Trail . Line <$> vectors f st
--   vectors f (Trail (Loop st offset)) = fmap Trail . Loop <$> vectors f st <*> vectors f offset
--   {-# INLINE vectors #-}
-- 
-- instance HasVectors (Point v n) (Point u n') where
--   vectors f (P v) = P <$> f v
--   {-# INLINE vectors #-}
-- 
-- instance HasVectors (FixedSegment v n) (FixedSegment u n') where
--   vectors f (FLinear p0 p1)      = FLinear <$> vectors f p0 <*> vectors f p1
--   vectors f (FCubic p0 p1 p2 p3) = FCubic <$> vectors f p0 <*> vectors f p1
--                                           <*> vectors f p2 <*> vectors f p3
--   {-# INLINE vectors #-}
-- 
-- instance (HasVectors a b) => HasVectors (Located a) (Located b) where
--   vectors f (Loc p a) = Loc <$> vectors f p <*> vectors f a
--   {-# INLINE vectors #-}
-- 
-- instance (Metric v, OrderedField n, Metric u, OrderedField n')
--     => HasVectors (Path v n) (Path u n') where
--   vectors = _Wrapped . traversed . vectors
--   {-# INLINE vectors #-}
-- 
-- floats2Doubles :: (V a ~ v, V a ~ V b, N a ~ Float, N b ~ Double, Functor v, HasVectors a b) => a -> b
-- floats2Doubles = over (vectors . mapped) float2Double
-- 
-- doubles2Floats :: (V a ~ v, V a ~ V b, N a ~ Double, N b ~ Float, Functor v, HasVectors a b) => a -> b
-- doubles2Floats = over (vectors . mapped) double2Float
-- 
-- | Type for holding linear maps. Note that these are not affine transforms so 
--   attemping apply a translation with 'LinearMap' will likely produce incorrect 
--   results.
newtype LinearMap v u n = LinearMap { linmap :: v n -> u n }
  deriving Typeable

toLinearMap :: Transformation v n -> LinearMap v v n
toLinearMap (Transformation (m :-: _) _ _) = LinearMap m

-- vmap :: (InSpace v n a, InSpace u n b, HasVectors a b) => LinearMap v u n -> a -> b
-- vmap (LinearMap m) = over vectors m

-- | Traversal over all the vectors of an object.
class HasVectors a where
  type Restrict a (v :: * -> *) n :: Constraint
  type Restrict a (v :: * -> *) n = ()

  vectors :: (Restrict a v n, Restrict a u n')
    => Traversal (a v n) (a u n') (v n) (u n')

instance HasVectors (Offset c) where
  vectors f (OffsetClosed v) = OffsetClosed <$> f v
  vectors _ OffsetOpen       = pure OffsetOpen
  {-# INLINE vectors #-}

instance HasVectors (Segment c) where
  vectors f (Linear offset)      = Linear <$> vectors f offset
  vectors f (Cubic v1 v2 offset) = Cubic <$> f v1 <*> f v2 <*> vectors f offset
  {-# INLINE vectors #-}

instance HasVectors SegTree where
  type Restrict SegTree v n = (Metric v, OrderedField n)

  vectors = _Wrapped . traverse' . vectors
  {-# INLINE vectors #-}

instance HasVectors (Trail' l) where
  type Restrict (Trail' l) v n = (Metric v, OrderedField n)

  vectors f (Line st)        = Line <$> vectors f st
  vectors f (Loop st offset) = Loop <$> vectors f st <*> vectors f offset
  {-# INLINE vectors #-}

instance HasVectors Trail where
  type Restrict Trail v n = (Metric v, OrderedField n)

  vectors f (Trail (Line st))        = Trail . Line <$> vectors f st
  vectors f (Trail (Loop st offset)) = fmap Trail . Loop <$> vectors f st <*> vectors f offset
  {-# INLINE vectors #-}

instance HasVectors Point where
  vectors f (P v) = P <$> f v
  {-# INLINE vectors #-}

instance HasVectors FixedSegment where
  vectors f (FLinear p0 p1)      = FLinear <$> vectors f p0 <*> vectors f p1
  vectors f (FCubic p0 p1 p2 p3) = FCubic <$> vectors f p0 <*> vectors f p1
                                          <*> vectors f p2 <*> vectors f p3
  {-# INLINE vectors #-}

data Situated' a v n where
  Situated  :: (V a ~ v, N a ~ n) => Point v n -> a v n -> Situated' a v n
  Situated' :: (V a ~ v, N a ~ n) => Point v n -> a -> Situated' a v n

newtype Canonical a (v :: * -> *) n = Canonical {uncanonical :: a}

mkCanonical :: a -> Canonical a (V a) (N a)
mkCanonical = Canonical
-- 
-- data Situated' a v n = Situated
--   { _loc      :: Point v n
--   , _situated :: a
--   } deriving Typeable

-- mkSituated :: (V a ~ v, N a ~ n) => Point v n -> a -> Situated' a v n
-- mkSituated = Situated

-- type Situated a v n = Situated' (a v n) v n

-- instance HasVectors (Canonical a) => HasVectors (Situated' a) where
--   type Restrict (Situated' a) v n = (V a ~ v, N a ~ n, Restrict (Canonical a) v n)
-- 
--   vectors f (Situated p a) = Situated <$> vectors f p <*> fmap uncanonical (vectors f (Canonical a))
--   {-# INLINE vectors #-}

instance HasVectors Path where
  type Restrict Path v n = (Metric v, OrderedField n)
  vectors = _Wrapped . traversed . locatedVectors
    where
      locatedVectors f (Loc p a) = Loc <$> vectors f p <*> vectors f a
  {-# INLINE vectors #-}

-- | Affine linear maps. Unlike Transformation these do not have to be 
-- invertable so we can map between spaces.
data AffineMap v u n = AffineMap
  { _preTransform  :: Transformation v n
  , _linearMap     :: LinearMap v u n
  , _postTransform :: Transformation u n
  } deriving Typeable

type Restricting v u n a = (Restrict a v n, Restrict a u n, AffineMappable a)

aapply :: (AffineMappable a, Restrict a v n, Restrict a u n, Additive u, Additive v, Num n)
  => AffineMap v u n -> a v n -> a u n
aapply (AffineMap pre (LinearMap l) post)
  = amap (apply post . l . apply pre)
         (papply post . over vectors l . papply pre)

class AffineMappable a where
  amap :: (Restrict a v n, Restrict a u n') => (v n -> u n') -> (Point v n -> Point u n') -> a v n -> a u n'

  -- amap :: (LTarget a u n' ~ b, LinContr a, LinContr b, V a ~ v, N a ~ n, V b ~ u, N b ~ n')
  --   => (v n -> u n') -> v n -> a -> b

instance AffineMappable (Offset c) where
  amap f _ = over vectors f

instance AffineMappable (Segment c) where
  amap f _ = over vectors f

instance AffineMappable SegTree where
  amap f _ = over vectors f

instance AffineMappable (Trail' l) where
  amap f _ = over vectors f

instance AffineMappable Trail where
  amap f _ = over vectors f

instance AffineMappable Point where
  amap _ f = f

instance AffineMappable FixedSegment where
  amap _ f (FLinear p0 p1)      = FLinear (f p0) (f p1)
  amap _ f (FCubic p0 p1 p2 p3) = FCubic (f p0) (f p1) (f p2) (f p3)

-- instance AffineMappable a => AffineMappable (Located a) where
--   amap fv fp (Loc p x) = Loc (fp p) (amap fv fp x)

instance AffineMappable Path where
  amap a v (Path path) = Path (map (amapLoc a v) path)
    where
    amapLoc fv fp (Loc p x) = Loc (fp p) (amap fv fp x)
  
