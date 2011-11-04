{-# LANGUAGE DeriveDataTypeable
           , DeriveFunctor
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TypeFamilies
           , ScopedTypeVariables
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

         -- * Operations on bounding boxes
       , contains, contains'
       , inside, inside', outside, outside'
       , union, intersection, unions, intersections
       ) where

import Control.Applicative ((<*>))
import Control.Monad (join, liftM2)
import Data.Map (Map, fromList, toList, fromDistinctAscList, toAscList)
import qualified Data.Foldable as F

import Data.Maybe (fromJust)

import Data.VectorSpace (VectorSpace, Scalar, AdditiveGroup, zeroV, negateV)
import Data.Basis (HasBasis, Basis, decompose, recompose, basisValue)

import Data.Data (Data)
import Data.Typeable (Typeable)

import Graphics.Rendering.Diagrams.Points (Point(..))
import Graphics.Rendering.Diagrams.HasOrigin (HasOrigin(..))
import Graphics.Rendering.Diagrams.Bounds (Boundable, boundary)
import Graphics.Rendering.Diagrams.V (V)

-- | A bounding box is an axis-aligned region determined
--   by two points indicating its \"lower\" and \"upper\" corners.
data BoundingBox v = BoundingBox (Point v) (Point v)
  deriving (Show, Read, Eq, Data, Typeable, Functor)

type instance V (BoundingBox v) = v

instance VectorSpace v => HasOrigin (BoundingBox v) where
  moveOriginTo p (BoundingBox p1 p2) = BoundingBox (moveOriginTo p p1)
                                                   (moveOriginTo p p2)

-- | Create a bounding box from any two opposite corners.
fromCorners
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => Point v -> Point v -> BoundingBox v
fromCorners u v = BoundingBox (toPoint (combineV min u v))
                              (toPoint (combineV max u v))

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

-- | Check whether a point is contained in a bounding box (including its edges).
contains
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> Point v -> Bool
contains (BoundingBox l h) p = F.and (combineV (<=) l p)
                            && F.and (combineV (<=) p h)

-- | Check whether a point is /strictly/ contained in a bounding box.
contains'
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> Point v -> Bool
contains' (BoundingBox l h) p = F.and (combineV (< ) l p)
                             && F.and (combineV (< ) p h)

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
inside   (BoundingBox ul uh) (BoundingBox vl vh) =  F.and (combineV (<=) uh vh)
                                                 && F.and (combineV (>=) ul vl)

-- | Test whether the first bounding box is /strictly/ contained
--   inside the second.
inside'
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
inside'  (BoundingBox ul uh) (BoundingBox vl vh) =  F.and (combineV (< ) uh vh)
                                                 && F.and (combineV (> ) ul vl)

-- | Test whether the first bounding box lies outside the second
--   (although they may intersect in their boundaries).
outside
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
outside  (BoundingBox ul uh) (BoundingBox vl vh) =  F.or  (combineV (<=) uh vl)
                                                 || F.or  (combineV (>=) ul vh)

-- | Test whether the first bounding box lies /strictly/ outside the second
--   (they do not intersect at all).
outside'
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
outside' (BoundingBox ul uh) (BoundingBox vl vh) =  F.or  (combineV (< ) uh vl)
                                                 || F.or  (combineV (> ) ul vh)

-- | Form the largest bounding box contained within this given two
--   bounding boxes, or @Nothing@ if the two bounding boxes do not
--   overlap at all.
intersection
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Maybe (BoundingBox v)
intersection u@(BoundingBox ul uh) v@(BoundingBox vl vh)
  | u `outside'` v = Nothing
  | otherwise = Just (fromCorners (toPoint (combineV max ul vl)) (toPoint (combineV min uh vh)))

-- | Form the smallest bounding box containing the given two bounding boxes.
union
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> BoundingBox v
union (BoundingBox ul uh) (BoundingBox vl vh) = BoundingBox (toPoint (combineV min ul vl)) (toPoint (combineV max uh vh))

-- internals using Map (Basis v) (Scalar v)
-- probably paranoia, but decompose might not always
--   1. contain basis elements whose component is zero
--   2. have basis elements in the same order

fromVector :: (HasBasis v, Ord (Basis v)) => v -> Map (Basis v) (Scalar v)
fromVector = fromList . decompose

toPoint :: HasBasis v => Map (Basis v) (Scalar v) -> Point v
toPoint = P . recompose . toList

combineV :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v)) => (Scalar v -> Scalar v -> a) -> Point v -> Point v -> Map (Basis v) a
combineV f (P u) (P v) = combineDefault zeroV zeroV f (fromVector u) (fromVector v)

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
