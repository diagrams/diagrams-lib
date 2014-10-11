{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Diagrams.LinearMap where

import Control.Lens
import Control.Applicative
import Data.FingerTree     as FT

import Diagrams.Segment
import Diagrams.Trail      hiding (offset)
import Diagrams.Path
import Diagrams.Located
import Diagrams.Core

import Linear.Affine
import Linear.Metric

-- | Traversal over all the vectors of an object.
class HasVectors a a' where
  vectors :: Traversal a a' (Vn a) (Vn a')

instance HasVectors (Offset c v n) (Offset c v' n') where
  vectors f (OffsetClosed v) = OffsetClosed <$> f v
  vectors _ OffsetOpen       = pure OffsetOpen

instance HasVectors (Segment c v n) (Segment c v' n') where
  vectors f (Linear offset)      = Linear <$> vectors f offset
  vectors f (Cubic v1 v2 offset) = Cubic <$> f v1 <*> f v2 <*> vectors f offset

instance (Metric v, OrderedField n, Metric v', OrderedField n')
    => HasVectors (SegTree v n) (SegTree v' n') where
  vectors = _Wrapped . traverse' . vectors

instance (Metric v, OrderedField n, Metric v', OrderedField n')
    => HasVectors (Trail' l v n) (Trail' l v' n') where
  vectors f (Line st)        = Line <$> vectors f st
  vectors f (Loop st offset) = Loop <$> vectors f st <*> vectors f offset

instance (Metric v, OrderedField n, Metric v', OrderedField n')
    => HasVectors (Trail v n) (Trail v' n') where
  vectors f (Trail (Line st))        = Trail . Line <$> vectors f st
  vectors f (Trail (Loop st offset)) = fmap Trail . Loop <$> vectors f st <*> vectors f offset

instance HasVectors (Point v n) (Point v' n') where
  vectors f (P v) = P <$> f v

instance (HasVectors a a') => HasVectors (Located a) (Located a') where
  vectors f (Loc p a) = Loc <$> vectors f p <*> vectors f a

instance (Metric v, OrderedField n, Metric v', OrderedField n')
    => HasVectors (Path v n) (Path v' n') where
  vectors = _Wrapped . traversed . vectors


