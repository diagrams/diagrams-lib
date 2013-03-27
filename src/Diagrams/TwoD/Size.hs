{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Size
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Utilities for working with sizes of two-dimensional objects.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Size
       (
         -- * Size and extent of diagrams in R2
         -- ** Computing sizes
         width, height, size2D, sizeSpec2D
       , extentX, extentY, center2D

         -- ** Specifying sizes
       , SizeSpec2D(..)
       , mkSizeSpec

       , requiredScaleT, requiredScale

         -- ** Changing the size of things
       , sized, sizedAs
       ) where

import           Diagrams.Core
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector

import           Control.Applicative  (liftA2, (<$>))
import           Control.Arrow        ((&&&), (***))

import           Data.Basis
import           Data.MemoTrie
import           Data.VectorSpace

------------------------------------------------------------
-- Computing diagram sizes
------------------------------------------------------------

-- | Compute the width of an enveloped object.
width :: ( Num b
         , AdditiveGroup b
         , Enveloped a
         , V a ~ V2 b
         ) => a -> Scalar (V2 b)
width = maybe 0 (negate . uncurry (-)) . extentX

-- | Compute the height of an enveloped object.
height :: ( Num b
          , AdditiveGroup b
          , Enveloped a
          , V a ~ V2 b
          ) => a -> Scalar (V2 b)
height = maybe 0 (negate . uncurry (-)) . extentY

-- | Compute the width and height of an enveloped object.
size2D :: ( Num b
          , AdditiveGroup b
          , Enveloped a
          , V a ~ (V2 b)
          ) => a -> (Scalar (V2 b), Scalar (V2 b))
size2D = width &&& height

-- | Compute the size of an enveloped object as a 'SizeSpec2D' value.
sizeSpec2D :: ( Num b
              , AdditiveGroup b
              , Enveloped a
              , V a ~ (V2 b)
              ) => a -> SizeSpec2D (Scalar (V2 b))
sizeSpec2D = uncurry Dims . size2D

-- | Compute the absolute  x-coordinate range of an enveloped object in
--   R2, in  the form (lo,hi).   Return @Nothing@ for objects  with an
--   empty envelope.
extentX :: ( Num b
           , AdditiveGroup b
           , Enveloped a
           , V a ~ V2 b
           ) => a -> Maybe (Scalar (V2 b), Scalar (V2 b))
extentX d = (\f -> (-f unit_X, f unitX)) <$> (appEnvelope . getEnvelope $ d)

-- | Compute the absolute y-coordinate range of an enveloped object in
--   R2, in the form (lo,hi).
extentY :: (Num b
           , AdditiveGroup b
           , Enveloped a
           , V a ~ V2 b
           ) => a -> Maybe (Scalar (V2 b), Scalar (V2 b))
extentY d = (\f -> (-f unit_Y, f unitY)) <$> (appEnvelope . getEnvelope $ d)

-- | Compute the point at the center (in the x- and y-directions) of a
--   enveloped object.  Return the origin for objects with an empty
--   envelope.
center2D :: ( Num b
            , AdditiveGroup b
            , Enveloped a
            , V a ~ V2 b
            ) => a -> P2 (Scalar (V2 b))
center2D = maybe origin (p2 . (mid *** mid)) . mm . (extentX &&& extentY)
  where mm = uncurry (liftA2 (,))
        mid = (/2) . uncurry (+)

------------------------------------------------------------
-- Size specifications
------------------------------------------------------------

-- | A specification of a (requested) rectangular size.
data SizeSpec2D a = Width a  -- ^ Specify an explicit
                             -- width. The height should be
                             -- determined automatically (so
                             -- as to preserve aspect ratio).
                  | Height a -- ^ Specify an explicit
                             -- height. The width should be
                             -- determined automatically (so
                             -- as to preserve aspect ratio).
                  | Dims a a -- ^ An explicit specification
                             -- of a width and height.
                  | Absolute -- ^ Absolute size: use whatever
                             -- size an object already has;
                             -- do not rescale.
  deriving (Eq, Ord, Show)

-- | Create a size specification from a possibly-specified width and
--   height.
mkSizeSpec :: Maybe a -> Maybe a -> SizeSpec2D a
mkSizeSpec Nothing  Nothing  = Absolute
mkSizeSpec (Just w) Nothing  = Width w
mkSizeSpec Nothing  (Just h) = Height h
mkSizeSpec (Just w) (Just h) = Dims w h

-- | @requiredScaleT spec sz@ returns a transformation (a uniform scale)
--   which can be applied to something of @size@ to make it fit the
--   requested size @spec@, without changing the aspect ratio.
requiredScaleT :: ( Ord a
                  , RealFloat a
                  , HasBasis a
                  , HasTrie (Basis a)
                  , Scalar a ~ a
                  ) => SizeSpec2D a -> (a, a) -> Transformation (V2 a)
requiredScaleT spec size = scaling (requiredScale spec size)

-- | @requiredScale spec sz@ returns a scaling factor necessary to
--   make something of size @sz@ fit the requested size @spec@,
--   without changing the aspect ratio.  Hence an explicit
--   specification of both dimensions may not be honored if the aspect
--   ratios do not match; in that case the scaling will be as large as
--   possible so that the object still fits within the requested size.
requiredScale :: ( Ord a
                 , RealFloat a
                 ) => SizeSpec2D a -> (a, a) -> a
requiredScale Absolute _    = 1
requiredScale (Width wSpec) (w,_)
  | wSpec == 0 || w == 0 = 1
  | otherwise            = wSpec / w
requiredScale (Height hSpec) (_,h)
  | hSpec == 0 || h == 0 = 1
  | otherwise            = hSpec / h
requiredScale (Dims wSpec hSpec) (w,h) = s
  where xscale  = wSpec / w
        yscale  = hSpec / h
        s'      = min xscale yscale
        s | isInfinite s' = 1
          | otherwise     = s'

-- | Uniformly scale any enveloped object so that it fits within the
--   given size.
sized :: ( Transformable a
         , Enveloped a
         , RealFloat b
         , HasBasis b
         , b ~ Scalar b
         , HasTrie (Basis b)
         , V a ~ (V2 b)
         ) => SizeSpec2D b -> a -> a
sized spec a = transform (requiredScaleT spec (size2D a)) a

-- | Uniformly scale an enveloped object so that it \"has the same
--   size as\" (fits within the width and height of) some other
--   object.
sizedAs :: ( Transformable a
           , RealFloat c
           , HasBasis c
           , HasTrie (Basis c)
           , c ~ Scalar c
           , Enveloped a, V a ~ (V2 c)
           , Enveloped b, V b ~ (V2 c)
           ) => b -> a -> a
sizedAs other = sized (sizeSpec2D other)
