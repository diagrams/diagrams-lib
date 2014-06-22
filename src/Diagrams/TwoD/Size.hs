{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}
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
       , sized, sizedAs, sizePair
       ) where

import           Diagrams.Core
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector

import           Control.Applicative  (liftA2, (<$>))
import           Control.Arrow        ((&&&), (***))
import           Data.Hashable        (Hashable)
import           Data.VectorSpace
import           GHC.Generics         (Generic)

------------------------------------------------------------
-- Computing diagram sizes
------------------------------------------------------------

-- | Compute the width of an enveloped object.
width :: (Enveloped a, R2Ish (V a)) => a -> Scalar (V a)
width = maybe 0 (negate . uncurry (-)) . extentX

-- | Compute the height of an enveloped object.
height :: (Enveloped a, R2Ish (V a)) => a -> Scalar (V a)
height = maybe 0 (negate . uncurry (-)) . extentY

-- | Compute the width and height of an enveloped object.
size2D :: (Enveloped a, R2Ish (V a)) => a -> (Scalar (V a), Scalar (V a))
size2D = width &&& height

-- | Compute the size of an enveloped object as a 'SizeSpec2D' value.
sizeSpec2D :: (Enveloped a, R2Ish (V a)) => a -> SizeSpec2D (Scalar (V a))
sizeSpec2D = uncurry Dims . size2D

-- | Compute the absolute  x-coordinate range of an enveloped object in
--   R2, in  the form (lo,hi).   Return @Nothing@ for objects  with an
--   empty envelope.
extentX :: (Enveloped a, R2Ish (V a)) => a -> Maybe (Scalar (V a), Scalar (V a))
extentX d = (\f -> (-f unit_X, f unitX)) <$> (appEnvelope . getEnvelope $ d)

-- | Compute the absolute y-coordinate range of an enveloped object in
--   R2, in the form (lo,hi).
extentY :: (Enveloped a, R2Ish (V a)) => a -> Maybe (Scalar (V a), Scalar (V a))
extentY d = (\f -> (-f unit_Y, f unitY)) <$> (appEnvelope . getEnvelope $ d)

-- | Compute the point at the center (in the x- and y-directions) of a
--   enveloped object.  Return the origin for objects with an empty
--   envelope.
center2D :: (Enveloped a, R2Ish (V a)) => a -> Point (V a)
center2D = maybe origin (p2 . (mid *** mid)) . mm . (extentX &&& extentY)
  where mm = uncurry (liftA2 (,))
        mid = (/2) . uncurry (+)

------------------------------------------------------------
-- Size specifications
------------------------------------------------------------

-- | A specification of a (requested) rectangular size.
data SizeSpec2D d = Width  !d       -- ^ Specify an explicit
                                      -- width. The height should be
                                      -- determined automatically (so
                                      -- as to preserve aspect ratio).
                | Height !d       -- ^ Specify an explicit
                                      -- height. The width should be
                                      -- determined automatically (so
                                      -- as to preserve aspect ratio).
                | Dims !d !d  -- ^ An explicit specification
                                      -- of a width and height.
                | Absolute            -- ^ Absolute size: use whatever
                                      -- size an object already has;
                                      -- do not rescale.
  deriving (Eq, Ord, Show, Generic)

instance Hashable d => Hashable (SizeSpec2D d)

-- | Create a size specification from a possibly-specified width and
--   height.
mkSizeSpec :: Maybe d -> Maybe d -> SizeSpec2D d
mkSizeSpec Nothing  Nothing  = Absolute
mkSizeSpec (Just w) Nothing  = Width w
mkSizeSpec Nothing  (Just h) = Height h
mkSizeSpec (Just w) (Just h) = Dims w h

-- | @requiredScaleT spec sz@ returns a transformation (a uniform scale)
--   which can be applied to something of size @sz@ to make it fit the
--   requested size @spec@, without changing the aspect ratio.
requiredScaleT :: (R2Ish v, Scalar v ~ d) => SizeSpec2D d -> (d, d) -> Transformation v
requiredScaleT spec size = scaling (requiredScale spec size)

-- | @requiredScale spec sz@ returns a scaling factor necessary to
--   make something of size @sz@ fit the requested size @spec@,
--   without changing the aspect ratio.  Hence an explicit
--   specification of both dimensions may not be honored if the aspect
--   ratios do not match; in that case the scaling will be as large as
--   possible so that the object still fits within the requested size.
requiredScale :: (RealFloat d) => SizeSpec2D d -> (d, d) -> d
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
sized :: (Transformable a, Enveloped a, R2Ish (V a))
      => SizeSpec2D (Scalar (V a)) -> a -> a
sized spec a = transform (requiredScaleT spec (size2D a)) a

-- | Uniformly scale an enveloped object so that it \"has the same
--   size as\" (fits within the width and height of) some other
--   object.
sizedAs :: ( Transformable a, Enveloped a, Enveloped b
           , R2Ish (V a), V a ~ V b
           )
        => b -> a -> a
sizedAs other = sized (sizeSpec2D other)

-- | Make width and height of `SizeSpec2D` into a tuple.
sizePair :: (Num d) => SizeSpec2D d -> (d, d)
sizePair (Width w')   = (w',w')
sizePair (Height h')  = (h',h')
sizePair (Dims w' h') = (w',h')
sizePair Absolute     = (100,100)
