{-# LANGUAGE DeriveDataTypeable
           , DeriveFunctor
           , FlexibleContexts
           , GeneralizedNewtypeDeriving
           , NoMonomorphismRestriction
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeFamilies
           , UndecidableInstances
  #-}
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
         BoundingBox()

         -- * Constructing bounding boxes
       , fromCorners, fromPoint, fromPoints
       , boundingBox

         -- * Queries on bounding boxes
       , getCorners, getAllCorners
       , boxExtents, boxTransform, boxFit
       , contains, contains'
       , inside, inside', outside, outside'

         -- * Operations on bounding boxes
       , union, intersection, unions, intersections
       ) where

import Control.Applicative ((<$>))
import Control.Monad (join, liftM2)
import Data.Map (Map, fromList, toList, fromDistinctAscList, toAscList)
import qualified Data.Foldable as F

import Data.Maybe (fromJust)

import Data.VectorSpace
-- (VectorSpace, Scalar, AdditiveGroup, zeroV, negateV, (^+^), (^-^))
import Data.Basis (HasBasis, Basis, decompose, recompose, basisValue)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..), Option(..))

import Data.Data (Data)
import Data.Typeable (Typeable)

import Graphics.Rendering.Diagrams.Points (Point(..))
import Graphics.Rendering.Diagrams.HasOrigin (HasOrigin(..))
import Graphics.Rendering.Diagrams.Envelope (Enveloped(..), appEnvelope)
import Graphics.Rendering.Diagrams.V (V)
import Graphics.Rendering.Diagrams.Transform
  (Transformation(..), Transformable(..), HasLinearMap, (<->))

-- Unexported utility newtype

newtype NonEmptyBoundingBox v = NonEmptyBoundingBox (Point v, Point v)
  deriving (Eq, Data, Typeable)

fromNonEmpty :: NonEmptyBoundingBox v -> BoundingBox v
fromNonEmpty = BoundingBox . Option . Just

fromMaybeEmpty :: ( HasBasis v, Ord (Basis v)
                  , s ~ Scalar v, AdditiveGroup (Scalar v), Ord (Scalar v))
  => Maybe (NonEmptyBoundingBox v) -> BoundingBox v
fromMaybeEmpty = maybe mempty fromNonEmpty

nonEmptyCorners :: NonEmptyBoundingBox v -> (Point v, Point v)
nonEmptyCorners (NonEmptyBoundingBox x) = x

instance (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
    => Semigroup (NonEmptyBoundingBox v) where
  (NonEmptyBoundingBox (ul, uh)) <> (NonEmptyBoundingBox (vl, vh))
    = NonEmptyBoundingBox
    $ mapT toPoint (combineP min ul vl, combineP max uh vh)


-- | A bounding box is an axis-aligned region determined by two points
--   indicating its \"lower\" and \"upper\" corners.  It can also represent
--   an empty bounding box - the points are wrapped in @Maybe@.
newtype BoundingBox v = BoundingBox (Option (NonEmptyBoundingBox v))
  deriving (Eq, Data, Typeable)

deriving instance ( HasBasis v, Ord (Basis v)
                  , AdditiveGroup (Scalar v), Ord (Scalar v)
                  ) => Semigroup (BoundingBox v)
deriving instance ( HasBasis v, Ord (Basis v)
                  , AdditiveGroup (Scalar v), Ord (Scalar v)
                  ) => Monoid (BoundingBox v)

type instance V (BoundingBox v) = v

-- Map a function on a homogenous 2-tuple. (unexported utility)
mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f (x, y) = (f x, f y)

instance ( VectorSpace v, HasBasis v, Ord (Basis v)
         , s ~ Scalar v, AdditiveGroup s, Ord s
         )  => HasOrigin (BoundingBox v) where
  moveOriginTo p b = fromMaybeEmpty
    $ (NonEmptyBoundingBox . mapT (moveOriginTo p) <$> getCorners b)

instance ( InnerSpace v, HasBasis v, Ord (Basis v)
         , s ~ Scalar v, AdditiveGroup s, Ord s, Floating s
         ) => Enveloped (BoundingBox v) where
  getEnvelope = getEnvelope . getAllCorners


-- | Create a bounding box from a point that is component-wise @(<=)@ than the
--   other.  If this is not the case, then @mempty@ is returned.
fromCorners
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => Point v -> Point v -> BoundingBox v
fromCorners l h
  | F.and (combineP (<=) l h) = fromNonEmpty $ NonEmptyBoundingBox (l, h)
  | otherwise = mempty

-- | Create a degenerate bounding \"box\" containing only a single point.
fromPoint
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => Point v -> BoundingBox v
fromPoint p = fromNonEmpty $ NonEmptyBoundingBox (p, p)

-- | Create the smallest bounding box containing all the given points.
fromPoints
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => [Point v] -> BoundingBox v
fromPoints = mconcat . map fromPoint

-- | Create a bounding box for any enveloped object (such as a diagram or path).
boundingBox :: forall a. (Enveloped a, HasBasis (V a), AdditiveGroup (V a), Ord (Basis (V a)))
            => a -> BoundingBox (V a)
boundingBox a = fromMaybeEmpty $ do
    env <- appEnvelope $ getEnvelope a
    let h = recompose $ map (\v -> (v,          env           $ basisValue v)) units
        l = recompose $ map (\v -> (v, negate . env . negateV $ basisValue v)) units
    return $ NonEmptyBoundingBox (P h, P l)
  where
    -- Might not work if 0-components aren't reported.
    units = map fst $ decompose (zeroV :: V a)

-- | Gets the lower and upper corners that define the bounding box.
getCorners :: BoundingBox v -> Maybe (Point v, Point v)
getCorners (BoundingBox p) = nonEmptyCorners <$> getOption p

-- | Computes all of the corners of the bounding box.
getAllCorners :: (HasBasis v, AdditiveGroup (Scalar v), Ord (Basis v))
              => BoundingBox v -> [Point v]
getAllCorners (BoundingBox (Option Nothing)) = []
getAllCorners (BoundingBox (Option (Just (NonEmptyBoundingBox (l, u)))))
  = map (P . recompose)
  -- Enumerate all combinations of selections of lower / higher values. 
  . mapM (\(b, (l', u')) -> [(b, l'), (b, u')])
  -- List of [(basis, (lower, upper))]
  . toList
  $ combineP (,) l u

-- | Get the size of the bounding box - the vector from the lesser to the greater
--   point.
boxExtents :: (AdditiveGroup v) => BoundingBox v -> v
boxExtents = maybe zeroV (\(P l, P h) -> h ^-^ l) . getCorners

-- | Create a transformation mapping points from one bounding box to the other.
boxTransform :: (AdditiveGroup v, HasLinearMap v, 
                 Fractional (Scalar v), AdditiveGroup (Scalar v), Ord (Basis v))
             => BoundingBox v -> BoundingBox v -> Maybe (Transformation v)
boxTransform u v = do
    ((P ul), _) <- getCorners u
    ((P vl), _) <- getCorners v
    let lin_map = box_scale (u, v) <-> box_scale (v, u)
        box_scale = combineV' (*) . uncurry (combineV' (/)) . mapT boxExtents
        combineV' f x = toVector . combineV f x
    return $ Transformation lin_map lin_map (vl ^-^ box_scale (u, v) ul)

-- | Transforms an enveloped thing to fit within a @BoundingBox@.  If it's
--   empty, then the result is also @mempty@.
boxFit :: (Enveloped a, Transformable a, Monoid a, Ord (Basis (V a)))
       => BoundingBox (V a) -> a -> a
boxFit b x = maybe mempty (`transform` x) $ boxTransform (boundingBox x) b

-- | Check whether a point is contained in a bounding box (including its edges).
contains
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> Point v -> Bool
contains b p = maybe False check $ getCorners b
  where
   check (l, h) = F.and (combineP (<=) l p)
               && F.and (combineP (<=) p h)

-- | Check whether a point is /strictly/ contained in a bounding box.
contains'
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> Point v -> Bool
contains' b p = maybe False check $ getCorners b
  where
   check (l, h) = F.and (combineP (<) l p)
               && F.and (combineP (<) p h)

-- | Compute the largest bounding box contained in all the given bounding
--   boxes. @Just@ @mempty@ is returned if there is no mutual intersection
--   intersections. @Nothing@ is returned if the empty list is provided.
intersections :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
              => [BoundingBox v] -> BoundingBox v
intersections [] = Nothing
intersections xs = Just $ foldr1 intersection xs

-- | Compute the smallest bounding box containing all of the given bounding
--   boxes. @mempty@ is yielded if passed the empty list.
unions :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
       => [BoundingBox v] -> BoundingBox v
unions = mconcat

-- | Test whether the first bounding box is contained inside
--   the second.
inside
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
inside u v = maybe False id $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.and (combineP (>=) ul vl)
        && F.and (combineP (<=) uh vh)

-- | Test whether the first bounding box is /strictly/ contained
--   inside the second.
inside'
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
inside' u v = maybe False id $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.and (combineP (>) ul vl)
        && F.and (combineP (<) uh vh)

-- | Test whether the first bounding box lies outside the second
--   (although they may intersect in their boundaries).
outside
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
outside u v = maybe True id $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.and (combineP (>=) ul vh)
        && F.and (combineP (<=) uh vl)

-- | Test whether the first bounding box lies /strictly/ outside the second
--   (they do not intersect at all).
outside'
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
outside' u v = maybe True id $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ F.and (combineP (>) ul vh)
        && F.and (combineP (<) uh vl)

-- | Form the largest bounding box contained within this given two
--   bounding boxes, or @Nothing@ if the two bounding boxes do not
--   overlap at all.
intersection
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> BoundingBox v
intersection u v = maybe mempty (uncurry fromCorners) $ do
  (ul, uh) <- getCorners u
  (vl, vh) <- getCorners v
  return $ mapT toPoint (combineP max ul vl, combineP min uh vh)

-- | Form the smallest bounding box containing the given two bound union.
union :: ( HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v) )
      => BoundingBox v -> BoundingBox v -> BoundingBox v
union = (<>)

-- internals using Map (Basis v) (Scalar v)
-- probably paranoia, but decompose might not always
--   1. contain basis elements whose component is zero
--   2. have basis elements in the same order

fromVector :: (HasBasis v, Ord (Basis v)) => v -> Map (Basis v) (Scalar v)
fromVector = fromList . decompose

toVector :: HasBasis v => Map (Basis v) (Scalar v) -> v
toVector = recompose . toList

toPoint :: HasBasis v => Map (Basis v) (Scalar v) -> Point v
toPoint = P . toVector

combineV :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v))
         => (Scalar v -> Scalar v -> a) -> v -> v -> Map (Basis v) a
combineV f u v = combineDefault zeroV zeroV f (fromVector u) (fromVector v)

combineP :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v))
         => (Scalar v -> Scalar v -> a) -> Point v -> Point v -> Map (Basis v) a
combineP f (P u) (P v) = combineV f u v

combineDefault :: Ord k => a -> b -> (a -> b -> c) -> Map k a -> Map k b -> Map k c
combineDefault a b f = combine g
  where
    g Nothing  Nothing  = f a b
    g Nothing  (Just y) = f a y
    g (Just x) Nothing  = f x b
    g (Just x) (Just y) = f x y

combine :: Ord k => (Maybe a -> Maybe b -> c) -> Map k a -> Map k b -> Map k c
combine f am bm = fromDistinctAscList $ merge (toAscList am) (toAscList bm)
  where
    merge [] [] = []
    merge ((x,a):xs) [] = (x, f (Just a) Nothing) : merge xs []
    merge [] ((y,b):ys) = (y, f Nothing (Just b)) : merge [] ys
    merge xs0@((x,a):xs) ys0@((y,b):ys) = case compare x y of
      LT -> (x, f (Just a) Nothing ) : merge xs ys0
      EQ -> (x, f (Just a) (Just b)) : merge xs ys
      GT -> (y, f Nothing  (Just b)) : merge xs0 ys