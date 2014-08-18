{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.BoundingBox
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Bounding boxes are not very compositional (/e.g./ it is not
-- possible to do anything sensible with them under rotation), so they
-- are not used in the diagrams core.  However, they do have their
-- uses; this module provides definitions and functions for working
-- with them.
--
-----------------------------------------------------------------------------

module Diagrams.BoundingBox
       ( -- * Bounding boxes
         BoundingBox

         -- * Constructing bounding boxes
       , emptyBox, fromCorners, fromPoint, fromPoints
       , boundingBox

         -- * Queries on bounding boxes
       , isEmptyBox
       , getCorners, getAllCorners
       , boxExtents , boxTransform, boxFit
       , contains, contains'
       , inside, inside', outside, outside'

         -- * Operations on bounding boxes
       , union, intersection
       ) where

import Data.Data      (Data, Typeable)
import Data.Foldable  as F
import Data.Maybe     (fromMaybe)
import Data.Monoid    (Monoid (..))
import Data.Semigroup (Option (..), Semigroup (..))

import Diagrams.Core.Envelope
import Diagrams.Core.HasOrigin (HasOrigin (..))
import Diagrams.Core.Transform
import Diagrams.Core.V

import Control.Applicative
import Data.Traversable    as T
import Linear.Affine
import Linear.Metric
import Linear.Vector

-- Unexported utility newtype

newtype NonEmptyBoundingBox v n = NonEmptyBoundingBox (Point v n, Point v n)
  deriving (Eq, Data, Typeable, Functor)

type instance V (NonEmptyBoundingBox v n) = v
type instance N (NonEmptyBoundingBox v n) = n

fromNonEmpty :: NonEmptyBoundingBox v n -> BoundingBox v n
fromNonEmpty = BoundingBox . Option . Just

fromMaybeEmpty :: Maybe (NonEmptyBoundingBox v n) -> BoundingBox v n
fromMaybeEmpty = maybe emptyBox fromNonEmpty

nonEmptyCorners :: NonEmptyBoundingBox v n -> (Point v n, Point v n)
nonEmptyCorners (NonEmptyBoundingBox x) = x

instance (Additive v, Ord n) => Semigroup (NonEmptyBoundingBox v n) where
  (NonEmptyBoundingBox (ul, uh)) <> (NonEmptyBoundingBox (vl, vh))
    = NonEmptyBoundingBox (liftU2 min ul vl, liftU2 max uh vh)

-- | A bounding box is an axis-aligned region determined by two points
--   indicating its \"lower\" and \"upper\" corners.  It can also represent
--   an empty bounding box - the points are wrapped in @Maybe@.
newtype BoundingBox v n = BoundingBox (Option (NonEmptyBoundingBox v n))
  deriving (Eq, Data, Typeable, Functor)

deriving instance (Additive v, Ord n) => Semigroup (BoundingBox v n)
deriving instance (Additive v, Ord n) => Monoid (BoundingBox v n)

type instance V (BoundingBox v n) = v
type instance N (BoundingBox v n) = n

-- Map a function on a homogenous 2-tuple. (unexported utility)
mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f (x, y) = (f x, f y)
{-# NOINLINE mapT #-}

instance (Additive v, Num n, Ord n) => HasOrigin (BoundingBox v n) where
  moveOriginTo p b
    = fromMaybeEmpty
    (NonEmptyBoundingBox . mapT (moveOriginTo p) <$> getCorners b)

instance (Metric v, Traversable v, OrderedField n)
          => Enveloped (BoundingBox v n) where
  getEnvelope = getEnvelope . getAllCorners

instance Show (v n) => Show (BoundingBox v n) where
  show
    = maybe "emptyBox" (\(l, u) -> "fromCorners " ++ show l ++ " " ++ show u)
    . getCorners

-- | An empty bounding box.  This is the same thing as @mempty@, but it doesn't
--   require the same type constraints that the @Monoid@ instance does.
emptyBox :: BoundingBox v n
emptyBox = BoundingBox $ Option Nothing

-- | Create a bounding box from a point that is component-wise @(<=)@ than the
--   other.  If this is not the case, then @mempty@ is returned.
fromCorners
  :: (Additive v, Foldable v, Ord n)
  => Point v n -> Point v n -> BoundingBox v n
fromCorners l h
  | F.and (liftI2 (<=) l h) = fromNonEmpty $ NonEmptyBoundingBox (l, h)
  | otherwise               = mempty

-- | Create a degenerate bounding \"box\" containing only a single point.
fromPoint :: Point v n -> BoundingBox v n
fromPoint p = fromNonEmpty $ NonEmptyBoundingBox (p, p)

-- | Create the smallest bounding box containing all the given points.
fromPoints :: (Additive v, Ord n) => [Point v n] -> BoundingBox v n
fromPoints = mconcat . map fromPoint

-- | Create a bounding box for any enveloped object (such as a diagram or path).
boundingBox :: (VN a ~ v n, Enveloped a, HasLinearMap v, Num n)
            => a -> BoundingBox v n
boundingBox a = fromMaybeEmpty $ do
  env <- (appEnvelope . getEnvelope) a
  let h = fmap env eye
      l = negated $ fmap (env . negated) eye
  return $ NonEmptyBoundingBox (P l, P h)

-- | Queries whether the BoundingBox is empty.
isEmptyBox :: BoundingBox v n -> Bool
isEmptyBox (BoundingBox (Option Nothing)) = True
isEmptyBox _                              = False

-- | Gets the lower and upper corners that define the bounding box.
getCorners :: BoundingBox v n -> Maybe (Point v n, Point v n)
getCorners (BoundingBox p) = nonEmptyCorners <$> getOption p
--
-- -- | Computes all of the corners of the bounding box.
getAllCorners :: (Additive v, Traversable v, Num n) => BoundingBox v n -> [Point v n]
getAllCorners (BoundingBox (Option Nothing)) = []
getAllCorners (BoundingBox (Option (Just (NonEmptyBoundingBox (l, u)))))
  = T.sequence (liftI2 (\a b -> [a,b]) l u)

-- | Get the size of the bounding box - the vector from the (component-wise)
--   lesser point to the greater point.
boxExtents :: (Additive v, Num n) => BoundingBox v n -> v n
boxExtents = maybe zero (uncurry (.-.)) . getCorners

-- | Create a transformation mapping points from one bounding box to the other.
boxTransform
  :: (Additive v, Fractional n)
  => BoundingBox v n -> BoundingBox v n -> Maybe (Transformation v n)
boxTransform u v = do
    (P ul, _) <- getCorners u
    (P vl, _) <- getCorners v
    let i  = s (v, u) <-> s (u, v)
        s = liftU2 (*) . uncurry (liftU2 (/)) . mapT boxExtents
    return $ Transformation i i (vl ^-^ s (v, u) ul)
    -- NOTE: Need to check this one

-- | Transforms an enveloped thing to fit within a @BoundingBox@.  If it's
--   empty, then the result is also @mempty@.
boxFit
  :: (VN a ~ v n, Enveloped a, Transformable a, Monoid a, HasLinearMap v, Num n)
  => BoundingBox (V a) (N a) -> a -> a
boxFit b x = maybe mempty (`transform` x) $ boxTransform (boundingBox x) b

-- | Check whether a point is contained in a bounding box (including its edges).
contains :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> Point v n -> Bool
contains b p = maybe False check $ getCorners b
  where
    check (l, h) = F.and (liftI2 (<=) l p)
                && F.and (liftI2 (<=) p h)

-- | Check whether a point is /strictly/ contained in a bounding box.
contains' :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> Point v n -> Bool
contains' b p = maybe False check $ getCorners b
  where
    check (l, h) = F.and (liftI2 (<) l p)
                && F.and (liftI2 (<) p h)

-- | Test whether the first bounding box is contained inside
--   the second.
inside :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> BoundingBox v n -> Bool
inside u v = fromMaybe False $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.and (liftI2 (>=) ul vl)
        && F.and (liftI2 (<=) uh vh)

-- | Test whether the first bounding box is /strictly/ contained
--   inside the second.
inside' :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> BoundingBox v n -> Bool
inside' u v = fromMaybe False $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.and (liftI2 (>) ul vl)
        && F.and (liftI2 (<) uh vh)

-- | Test whether the first bounding box lies outside the second
--   (although they may intersect in their boundaries).
outside :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> BoundingBox v n -> Bool
outside u v = fromMaybe True $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.or (liftI2 (<=) uh vl)
        || F.or (liftI2 (>=) ul vh)

-- | Test whether the first bounding box lies /strictly/ outside the second
--   (they do not intersect at all).
outside' :: (Additive v, Foldable v, Ord n) => BoundingBox v n -> BoundingBox v n -> Bool
outside' u v = fromMaybe True $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.or (liftI2 (<) uh vl)
        || F.or (liftI2 (>) ul vh)

-- | Form the largest bounding box contained within this given two
--   bounding boxes, or @Nothing@ if the two bounding boxes do not
--   overlap at all.
intersection
  :: (Additive v, Foldable v, Ord n)
  => BoundingBox v n -> BoundingBox v n -> BoundingBox v n
intersection u v = maybe mempty (uncurry fromCorners) $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return (liftI2 max ul vl, liftI2 min uh vh)

-- | Form the smallest bounding box containing the given two bound union.  This
--   function is just an alias for @mappend@.
union :: (Additive v, Ord n) => BoundingBox v n -> BoundingBox v n -> BoundingBox v n
union = mappend

