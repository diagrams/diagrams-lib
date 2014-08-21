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
       , spec2D

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
import           GHC.Generics         (Generic)

import Linear.Vector
import Control.Lens (Iso', iso)

------------------------------------------------------------
-- Computing diagram sizes
------------------------------------------------------------

-- | Compute the width of an enveloped object.
width :: (Vn a ~ V2 n, Enveloped a) => a -> n
width = maybe 0 (negate . uncurry (-)) . extentX

-- | Compute the height of an enveloped object.
height :: (Vn a ~ V2 n, Enveloped a) => a -> n
height = maybe 0 (negate . uncurry (-)) . extentY

-- | Compute the width and height of an enveloped object.
size2D :: (Vn a ~ V2 n, Enveloped a) => a -> (n, n)
size2D = width &&& height

-- | Compute the size of an enveloped object as a 'SizeSpec2D' value.
sizeSpec2D :: (Vn a ~ V2 n, Enveloped a) => a -> SizeSpec2D n
sizeSpec2D = uncurry Dims . size2D

-- | Compute the absolute  x-coordinate range of an enveloped object in
--   R2, in  the form (lo,hi).   Return @Nothing@ for objects  with an
--   empty envelope.
extentX :: (Vn a ~ V2 n, Enveloped a) => a -> Maybe (n, n)
extentX d = (\f -> (-f unit_X, f unitX)) <$> (appEnvelope . getEnvelope $ d)

-- | Compute the absolute y-coordinate range of an enveloped object in
--   R2, in the form (lo,hi).
extentY :: (Vn a ~ V2 n, Enveloped a) => a -> Maybe (n, n)
extentY d = (\f -> (-f unit_Y, f unitY)) <$> (appEnvelope . getEnvelope $ d)

-- | Compute the point at the center (in the x- and y-directions) of a
--   enveloped object.  Return the origin for objects with an empty
--   envelope.
center2D :: (Vn a ~ V2 n, Enveloped a) => a -> Point V2 n
center2D = maybe origin (p2 . (mid *** mid)) . mm . (extentX &&& extentY)
  where mm = uncurry (liftA2 (,))
        mid = (/2) . uncurry (+)

------------------------------------------------------------
-- Size specifications
------------------------------------------------------------

-- | A specification of a (requested) rectangular size.
data SizeSpec2D n = Width  !n  -- ^ Specify an explicit
                               -- width. The height should be
                               -- determined automatically (so
                               -- as to preserve aspect ratio).
                  | Height !n  -- ^ Specify an explicit
                               -- height. The width should be
                               -- determined automatically (so
                               -- as to preserve aspect ratio).
                  | Dims !n !n -- ^ An explicit specification
                               -- of a width and height.
                  | Absolute   -- ^ Absolute size: use whatever
                               -- size an object already has;
                               -- do not rescale.
  deriving (Eq, Ord, Show, Generic)

instance Hashable n => Hashable (SizeSpec2D n)

-- | Create a size specification from a possibly-specified width and
--   height.
mkSizeSpec :: Maybe d -> Maybe d -> SizeSpec2D d
mkSizeSpec Nothing  Nothing  = Absolute
mkSizeSpec (Just w) Nothing  = Width w
mkSizeSpec Nothing  (Just h) = Height h
mkSizeSpec (Just w) (Just h) = Dims w h

-- | Isomorphism from 'SizeSpec2D' to @(Maybe width, Maybe height)@.
spec2D :: Iso' (SizeSpec2D n) (Maybe n, Maybe n)
spec2D = iso getter (uncurry mkSizeSpec)
  where getter (Width w)  = (Just w, Nothing)
        getter (Height h) = (Nothing, Just h)
        getter (Dims w h) = (Just w, Just h)
        getter Absolute   = (Nothing, Nothing)

-- | @requiredScaleT spec sz@ returns a transformation (a uniform scale)
--   which can be applied to something of size @sz@ to make it fit the
--   requested size @spec@, without changing the aspect ratio.
requiredScaleT
  :: (Additive v, RealFloat n)
  => SizeSpec2D n -> (n, n) -> Transformation v n
requiredScaleT spec size = scaling (requiredScale spec size)
-- is requiredScaling a more consistent name?

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
sized :: (Vn a ~ V2 n, Transformable a, Enveloped a, RealFloat n)
      => SizeSpec2D n -> a -> a
sized spec a = transform (requiredScaleT spec (size2D a)) a

-- | Uniformly scale an enveloped object so that it \"has the same
--   size as\" (fits within the width and height of) some other
--   object.
sizedAs :: (Vn a ~ V2 n, Vn a ~ Vn b, Transformable a,
            Enveloped a, Enveloped b, RealFloat n)
        => b -> a -> a
sizedAs other = sized (sizeSpec2D other)

-- | Make width and height of `SizeSpec2D` into a tuple.
sizePair :: (Num d) => SizeSpec2D d -> (d, d)
sizePair (Width w')   = (w',w')
sizePair (Height h')  = (h',h')
sizePair (Dims w' h') = (w',h')
sizePair Absolute     = (100,100)

