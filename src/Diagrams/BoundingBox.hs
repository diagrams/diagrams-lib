{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, NoMonomorphismRestriction, TypeFamilies #-}
module BoundingBox
 ( BoundingBox(), boundingBox, fromPoint, fromPoints
 , contains, contains'
 , inside, inside', outside, outside'
 , union, intersection, unions, intersections
 ) where

import Prelude hiding (and, or)
import Control.Monad (join, liftM2)
import Data.Map (Map, fromList, toList, fromDistinctAscList, toAscList, fold)
import Data.VectorSpace (VectorSpace, Scalar, AdditiveGroup, zeroV)
import Data.Basis (HasBasis, Basis, decompose, recompose)
import Data.AffineSpace ((.-.))

import Data.Data (Data)
import Data.Typeable (Typeable)

import Graphics.Rendering.Diagrams.Points (Point, origin)
import Graphics.Rendering.Diagrams.V (V)

data BoundingBox v = BoundingBox v v
  deriving (Show, Read, Eq, Data, Typeable)

type instance V (BoundingBox v) = v

boundingBox
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => v -> v -> BoundingBox v
boundingBox u v = BoundingBox (toVector (combineV min u v)) (toVector (combineV max u v))

fromPoint
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => Point v -> BoundingBox v
fromPoint p = let v = p .-. origin in BoundingBox v v

fromPoints
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => [Point v] -> Maybe (BoundingBox v)
fromPoints [] = Nothing
fromPoints ps = Just . foldr1 union . map fromPoint $ ps

contains, contains'
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> Point v -> Bool
contains  (BoundingBox l h) p = and (combineV (<=) l v) && and (combineV (<=) v h) where v = p .-. origin
contains' (BoundingBox l h) p = and (combineV (< ) l v) && and (combineV (< ) v h) where v = p .-. origin

unions, intersections
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => [BoundingBox v] -> Maybe (BoundingBox v)
unions [] = Nothing
unions ps = Just . foldr1 union $ ps
intersections [] = Nothing
intersections ps = foldr1 ((join .) . liftM2 intersection) (map Just ps)

inside, inside', outside, outside'
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Bool
inside   (BoundingBox ul uh) (BoundingBox vl vh) =  and (combineV (<=) uh vh) && and (combineV (>=) ul vl)
inside'  (BoundingBox ul uh) (BoundingBox vl vh) =  and (combineV (< ) uh vh) && and (combineV (> ) ul vl)
outside  (BoundingBox ul uh) (BoundingBox vl vh) =  or  (combineV (<=) uh vl) || or  (combineV (>=) ul vh)
outside' (BoundingBox ul uh) (BoundingBox vl vh) =  or  (combineV (< ) uh vl) || or  (combineV (> ) ul vh)

intersection
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> Maybe (BoundingBox v)
intersection u@(BoundingBox ul uh) v@(BoundingBox vl vh)
  | u `outside'` v = Nothing
  | otherwise = Just (boundingBox (toVector (combineV max ul vl)) (toVector (combineV min uh vh)))

union
  :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v), Ord (Scalar v))
  => BoundingBox v -> BoundingBox v -> BoundingBox v
union (BoundingBox ul uh) (BoundingBox vl vh) = BoundingBox (toVector (combineV min ul vl)) (toVector (combineV max uh vh))

-- internals using Map (Basis v) (Scalar v)
-- probably paranoia, but decompose might not always
--   1. contain basis elements whose component is zero
--   2. have basis elements in the same order

fromVector :: (HasBasis v, Ord (Basis v)) => v -> Map (Basis v) (Scalar v)
fromVector = fromList . decompose

toVector :: HasBasis v => Map (Basis v) (Scalar v) -> v
toVector = recompose . toList

combineV :: (HasBasis v, Ord (Basis v), AdditiveGroup (Scalar v)) => (Scalar v -> Scalar v -> a) -> v -> v -> Map (Basis v) a
combineV f u v = combineDefault zeroV zeroV f (fromVector u) (fromVector v)

combineDefault :: Ord k => a -> b -> (a -> b -> c) -> Map k a -> Map k b -> Map k c
combineDefault a b f = combine g
  where
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

and, or :: Map k Bool -> Bool
and = fold (&&) True
or  = fold (||) False
