{-# LANGUAGE DeriveDataTypeable
           , DeriveFunctor
           , FlexibleContexts
           , NoMonomorphismRestriction
           , ScopedTypeVariables
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
-- Definitions and functions for working with bounding boxes.
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

import Control.Applicative ((<*>))
import Control.Monad (join, liftM2)
import Data.Map (Map, fromList, toList, fromDistinctAscList, toAscList)
import qualified Data.Foldable as F

import Data.Maybe (fromJust)

import Data.VectorSpace
-- (VectorSpace, Scalar, AdditiveGroup, zeroV, negateV, (^+^), (^-^))
import Data.Basis (HasBasis, Basis, decompose, recompose, basisValue)

import Data.Data (Data)
import Data.Typeable (Typeable)

import Graphics.Rendering.Diagrams.Points (Point(..))
import Graphics.Rendering.Diagrams.HasOrigin (HasOrigin(..))
import Graphics.Rendering.Diagrams.Bounds (Boundable(..), boundary)
import Graphics.Rendering.Diagrams.V (V)
import Graphics.Rendering.Diagrams.Transform
  (Transformation(..), Transformable(..), HasLinearMap, (<->), fromLinear)

-- | A bounding box is an axis-aligned region determined
--   by two points indicating its \"lower\" and \"upper\" corners.
data BoundingBox v = BoundingBox (Point v) (Point v)
  deriving (Show, Read, Eq, Data, Typeable, Functor)

type instance V (BoundingBox v) = v

instance VectorSpace v => HasOrigin (BoundingBox v) where
  moveOriginTo p (BoundingBox p1 p2) = BoundingBox (moveOriginTo p p1)
                                                   (moveOriginTo p p2)

instance ( InnerSpace v, Floating (Scalar v), Ord (Scalar v), AdditiveGroup (Scalar v)
         , HasBasis v, Ord (Basis v)
         ) => Boundable (BoundingBox v) where
  getBounds = getBounds . getAllCorners

-- | Create a bounding box from any two opposite corners.
fromCorners
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => Point v -> Point v -> BoundingBox v
fromCorners u v = BoundingBox (toPoint (combineP min u v))
                              (toPoint (combineP max u v))

-- | Create a degenerate bounding \"box\" containing only a single point.
fromPoint
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => Point v -> BoundingBox v
fromPoint p = BoundingBox p p

-- | Create the smallest bounding box containing all the given points.
fromPoints
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => [Point v] -> Maybe (BoundingBox v)
fromPoints = unions . map fromPoint

-- | Create a bounding box for any boundable object (such as a diagram or path).
boundingBox :: forall a. (Boundable a, HasBasis (V a), Ord (Basis (V a)))
            => a -> BoundingBox (V a)
boundingBox a = fromJust . fromPoints . map (flip boundary a) $ [id, negateV] <*> units
  where units = map (basisValue . fst) (decompose (zeroV :: V a))

-- | Gets the lower and upper corners that define the bounding box.
getCorners :: BoundingBox v -> (Point v, Point v)
getCorners (BoundingBox l u) = (l, u)

{-
Ord (Data.Basis.Basis b),
      Data.AdditiveGroup.AdditiveGroup (Data.VectorSpace.Scalar b),
      Data.Basis.HasBasis b,
      Data.Basis.HasBasis v,
      Data.Basis.Basis v ~ Data.Basis.Basis b,
      Data.VectorSpace.Scalar v ~ Data.VectorSpace.Scalar b) =>
-}

-- | Computes all of the corners of the bounding box.
getAllCorners :: (HasBasis v, AdditiveGroup (Scalar v), Ord (Basis v))
              => BoundingBox v -> [Point v]
getAllCorners (BoundingBox l u)
  = map (P . recompose)
  -- Enumerate all combinations of selections of lower / higher values. 
  . sequence . map (\(b, (x, y)) -> [(b, x), (b, y)])
  . toList $ combineP (,) l u

-- | Get the size of the bounding box - the vector from the lesser to the greater
--   point.
boxExtents :: (AdditiveGroup v) => BoundingBox v -> v
boxExtents (BoundingBox (P l) (P h)) = h ^-^ l

-- | Create a transformation mapping points from one bounding box to the other.
boxTransform :: (AdditiveGroup v, HasLinearMap v, 
                 Fractional (Scalar v), AdditiveGroup (Scalar v), Ord (Basis v))
             => BoundingBox v -> BoundingBox v -> Transformation v
  --TODO: is this right??
boxTransform a@(BoundingBox (P l1) _) b@(BoundingBox (P l2) _)
  = Transformation s s (l2 ^-^ boxTrans a b l1)
 where
  s = boxTrans a b <-> boxTrans b a
  boxTrans a b = vcombineV (*) (vcombineV (/) (boxExtents b) (boxExtents a))
  vcombineV f x = toVector . combineV f x

-- | Transforms a boundable thing to fit within a @BoundingBox@.
boxFit :: (Boundable a, Transformable a, Ord (Basis (V a)))
       => BoundingBox (V a) -> a -> a
boxFit b x = transform (boxTransform (boundingBox x) b) x

-- | Check whether a point is contained in a bounding box (including its edges).
contains
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> Point v -> Bool
contains (BoundingBox l h) p = F.and (combineP (<=) l p)
                            && F.and (combineP (<=) p h)

-- | Check whether a point is /strictly/ contained in a bounding box.
contains'
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> Point v -> Bool
contains' (BoundingBox l h) p = F.and (combineP (< ) l p)
                             && F.and (combineP (< ) p h)

-- | Compute the smallest bounding box containing all the given
--   bounding boxes (or @Nothing@ if the list is empty).
unions
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => [BoundingBox v] -> Maybe (BoundingBox v)
unions [] = Nothing
unions ps = Just . foldr1 union $ ps

-- | Compute the largest bounding box contained in all the given
--   bounding boxes (or @Nothing@ is the list is empty).
intersections
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => [BoundingBox v] -> Maybe (BoundingBox v)
intersections [] = Nothing
intersections ps = foldr1 ((join .) . liftM2 intersection) (map Just ps)

-- | Test whether the first bounding box is contained inside
--   the second.
inside
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
inside   (BoundingBox ul uh) (BoundingBox vl vh) =  F.and (combineP (<=) uh vh)
                                                 && F.and (combineP (>=) ul vl)

-- | Test whether the first bounding box is /strictly/ contained
--   inside the second.
inside'
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
inside'  (BoundingBox ul uh) (BoundingBox vl vh) =  F.and (combineP (< ) uh vh)
                                                 && F.and (combineP (> ) ul vl)

-- | Test whether the first bounding box lies outside the second
--   (although they may intersect in their boundaries).
outside
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
outside  (BoundingBox ul uh) (BoundingBox vl vh) =  F.or  (combineP (<=) uh vl)
                                                 || F.or  (combineP (>=) ul vh)

-- | Test whether the first bounding box lies /strictly/ outside the second
--   (they do not intersect at all).
outside'
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
outside' (BoundingBox ul uh) (BoundingBox vl vh) =  F.or  (combineP (< ) uh vl)
                                                 || F.or  (combineP (> ) ul vh)

-- | Form the largest bounding box contained within this given two
--   bounding boxes, or @Nothing@ if the two bounding boxes do not
--   overlap at all.
intersection
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Maybe (BoundingBox v)
intersection u@(BoundingBox ul uh) v@(BoundingBox vl vh)
  | u `outside'` v = Nothing
  | otherwise = Just (fromCorners (toPoint (combineP max ul vl)) (toPoint (combineP min uh vh)))

-- | Form the smallest bounding box containing the given two bounding boxes.
union
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> BoundingBox v
union (BoundingBox ul uh) (BoundingBox vl vh) = BoundingBox (toPoint (combineP min ul vl)) (toPoint (combineP max uh vh))

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