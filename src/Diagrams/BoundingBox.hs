{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.BoundingBox
-- Copyright   :  (c) 2011-2015 diagrams-lib team (see LICENSE)
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
  , boxExtents, boxCenter
  , mCenterPoint, centerPoint
  , boxTransform, boxFit
  , contains, contains'
  , inside, inside', outside, outside'

  , boxGrid

    -- * Operations on bounding boxes
  , union, intersection
  ) where

import           Control.Lens            (AsEmpty (..), Each (..), nearly)
import           Data.Foldable           as F
import           Data.Maybe              (fromMaybe)
import           Data.Semigroup
import           Text.Read

import           Diagrams.Align
import           Diagrams.Core
import           Diagrams.Core.Transform
import           Diagrams.Path
import           Diagrams.Query
import           Diagrams.ThreeD.Shapes  (cube)
import           Diagrams.ThreeD.Types
import           Diagrams.TwoD.Path      ()
import           Diagrams.TwoD.Shapes
import           Diagrams.TwoD.Types

import           Control.Applicative
import           Data.Traversable        as T
import           Linear.Affine
import           Linear.Metric
import           Linear.Vector           hiding (lerp)
import           Linear.Vector.Compat    (lerp)

-- Unexported utility newtype

newtype NonEmptyBoundingBox v n = NonEmptyBoundingBox (Point v n, Point v n)
  deriving (Eq, Functor)

type instance V (NonEmptyBoundingBox v n) = v
type instance N (NonEmptyBoundingBox v n) = n

fromNonEmpty :: NonEmptyBoundingBox v n -> BoundingBox v n
fromNonEmpty = BoundingBox . Just

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
newtype BoundingBox v n = BoundingBox (Maybe (NonEmptyBoundingBox v n))
  deriving (Eq, Functor)

deriving instance (Additive v, Ord n) => Semigroup (BoundingBox v n)
deriving instance (Additive v, Ord n) => Monoid (BoundingBox v n)

instance AsEmpty (BoundingBox v n) where
  _Empty = nearly emptyBox isEmptyBox

-- | Only valid if the second point is not smaller than the first.
instance (Additive v', Foldable v', Ord n') =>
    Each (BoundingBox v n) (BoundingBox v' n') (Point v n) (Point v' n') where
  each f (getCorners -> Just (l, u)) = fromCorners <$> f l <*> f u
  each _ _                           = pure emptyBox

type instance V (BoundingBox v n) = v
type instance N (BoundingBox v n) = n

-- Map a function on a homogeneous 2-tuple. (unexported utility)
mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f (x, y) = (f x, f y)

instance (Additive v, Num n) => HasOrigin (BoundingBox v n) where
  moveOriginTo p b
    = fromMaybeEmpty
    (NonEmptyBoundingBox . mapT (moveOriginTo p) <$> getCorners b)

instance (Additive v, Foldable v, Ord n)
     => HasQuery (BoundingBox v n) Any where
  getQuery bb = Query $ Any . contains bb

instance (Metric v, Traversable v, OrderedField n)
     => Enveloped (BoundingBox v n) where
  getEnvelope = getEnvelope . getAllCorners

-- Feels like cheating.
-- Should be possible to generalise this.
instance RealFloat n => Traced (BoundingBox V2 n) where
  getTrace = getTrace
           . ((`boxFit` rect 1 1) . boundingBox :: Envelope V2 n -> Path V2 n)
           . getEnvelope

instance TypeableFloat n => Traced (BoundingBox V3 n) where
  getTrace bb = foldMap (\tr -> getTrace $ transform tr cube) $
                boxTransform (boundingBox cube) bb

instance (Metric v, Traversable v, OrderedField n) => Alignable (BoundingBox v n) where
  defaultBoundary = envelopeP

instance Show (v n) => Show (BoundingBox v n) where
  showsPrec d b = case getCorners b of
    Just (l, u) -> showParen (d > 10) $
      showString "fromCorners " . showsPrec 11 l . showChar ' ' . showsPrec 11 u
    Nothing     -> showString "emptyBox"

instance Read (v n) => Read (BoundingBox v n) where
  readPrec = parens $
    (do
      Ident "emptyBox" <- lexP
      pure emptyBox
    ) <|>
    (prec 10 $ do
      Ident "fromCorners" <- lexP
      l <- step readPrec
      h <- step readPrec
      pure . fromNonEmpty $ NonEmptyBoundingBox (l, h)
    )

-- | An empty bounding box.  This is the same thing as @mempty@, but it doesn't
--   require the same type constraints that the @Monoid@ instance does.
emptyBox :: BoundingBox v n
emptyBox = BoundingBox Nothing

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
boundingBox :: (InSpace v n a, HasBasis v, Enveloped a)
            => a -> BoundingBox v n
boundingBox a = fromMaybeEmpty $ do
  env <- (appEnvelope . getEnvelope) a
  let h = fmap env eye
      l = negated $ fmap (env . negated) eye
  return $ NonEmptyBoundingBox (P l, P h)

-- | Queries whether the BoundingBox is empty.
isEmptyBox :: BoundingBox v n -> Bool
isEmptyBox (BoundingBox Nothing) = True
isEmptyBox _                              = False

-- | Gets the lower and upper corners that define the bounding box.
getCorners :: BoundingBox v n -> Maybe (Point v n, Point v n)
getCorners (BoundingBox p) = nonEmptyCorners <$> p

-- | Computes all of the corners of the bounding box.
getAllCorners :: (Additive v, Traversable v) => BoundingBox v n -> [Point v n]
getAllCorners (BoundingBox Nothing) = []
getAllCorners (BoundingBox (Just (NonEmptyBoundingBox (l, u))))
  = T.sequence (liftI2 (\a b -> [a,b]) l u)

-- | Get the size of the bounding box - the vector from the (component-wise)
--   lesser point to the greater point.
boxExtents :: (Additive v, Num n) => BoundingBox v n -> v n
boxExtents = maybe zero (\(l,u) -> u .-. l) . getCorners

-- | Get the center point in a bounding box.
boxCenter :: (Additive v, Fractional n) => BoundingBox v n -> Maybe (Point v n)
boxCenter = fmap (uncurry (lerp 0.5)) . getCorners

-- | Get the center of a the bounding box of an enveloped object, return
--   'Nothing' for object with empty envelope.
mCenterPoint :: (InSpace v n a, HasBasis v, Enveloped a)
            => a -> Maybe (Point v n)
mCenterPoint = boxCenter . boundingBox

-- | Get the center of a the bounding box of an enveloped object, return
--   the origin for object with empty envelope.
centerPoint :: (InSpace v n a, HasBasis v, Enveloped a)
            => a -> Point v n
centerPoint = fromMaybe origin . mCenterPoint

-- | Create a transformation mapping points from one bounding box to the
--   other. Returns 'Nothing' if either of the boxes are empty.
boxTransform
  :: (Additive v, Fractional n)
  => BoundingBox v n -> BoundingBox v n -> Maybe (Transformation v n)
boxTransform u v = do
  (P ul, _) <- getCorners u
  (P vl, _) <- getCorners v
  let i  = s (v, u) <-> s (u, v)
      s = liftU2 (*) . uncurry (liftU2 (/)) . mapT boxExtents
  return $ Transformation i i (vl ^-^ s (v, u) ul)

-- | Transforms an enveloped thing to fit within a @BoundingBox@.  If the
--   bounding box is empty, then the result is also @mempty@.
boxFit
  :: (InSpace v n a, HasBasis v, Enveloped a, Transformable a, Monoid a)
  => BoundingBox v n -> a -> a
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

-- | @boxGrid f box@ returns a grid of regularly spaced points inside
--   the box, such that there are @(1/f)@ points along each dimension.
--   For example, for a 3D box with corners at (0,0,0) and (2,2,2),
--   @boxGrid 0.1@ would yield a grid of approximately 1000 points (it
--   might actually be @11^3@ instead of @10^3@) spaced @0.2@ units
--   apart.
boxGrid
  :: (Traversable v, Additive v, Num n, Enum n)
  => n -> BoundingBox v n -> [Point v n]
boxGrid f = maybe [] (sequenceA . uncurry (liftI2 mkRange)) . getCorners
  where
    mkRange lo hi = [lo, (1-f)*lo + f*hi .. hi]

    -- liftA2 mkRange on the two corner points creates a (Point V2
    -- [n]), where each component is the range of values for that
    -- dimension.  sequenceA then yields a grid of type [Point V2 n].
