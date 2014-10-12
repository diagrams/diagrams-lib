{-# LANGUAGE CPP                  #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable   #-}
#endif
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Size
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Utilities for working with sizes of two-dimensional objects.
--
-----------------------------------------------------------------------------
module Diagrams.Size
  ( SizeSpec
  , sizeSpec
  , mkSpec
	, dims
	, absolute
	, getSpec
	, requiredScale
  , requiredScaling
	, sized
	, sizedAs
	, getSize
  , specSize
  ) where

import           Control.Applicative
import           Control.Lens         hiding (transform)
import           Control.Monad
import           Data.Foldable        as F
import           Data.Maybe
import           Data.Typeable
import           Data.Hashable
import           GHC.Generics         (Generic)

import           Diagrams.Core

import           Linear.Vector

------------------------------------------------------------
-- Computing diagram sizes
------------------------------------------------------------

-- | Compute the point in the center of the bounding box of the enveloped object.
-- centerPoint :: (InSpace v n a, Enveloped a, HasLinearMap v, HasBasis v) => a -> Maybe (Point v n)
-- centerPoint = fmap (uncurry $ lerp 0.5) . getCorners . boundingBox

-- | SizeSpec for a
newtype SizeSpec v n = SizeSpec (v n)
  deriving (
#if __GLASGOW_HASKELL__ >= 707
  Typeable,
#endif
  Functor,
  Generic,
  Hashable,
  Show)

#if __GLASGOW_HASKELL__ < 707
instance forall v. Typeable1 v => Typeable1 (SizeSpec v) where
  typeOf1 _ = mkTyConApp (mkTyCon3 "diagrams-lib" "Diagrams.Size" "Size") [] `mkAppTy`
              typeOf1 (undefined :: v n)
#endif

type instance V (SizeSpec v n) = v
type instance N (SizeSpec v n) = n

sizeSpec :: (InSpace v n a, Enveloped a, HasBasis v) => a -> SizeSpec v n
sizeSpec = dims . size

-- | Retrieve a size spec as a vector of maybe values. Only positive sizes are
--   returned.
getSpec :: (Functor v, Num n, Ord n) => SizeSpec v n -> v (Maybe n)
getSpec (SizeSpec sp) = mfilter (>0) . Just <$> sp

-- | Make a 'SizeSpec' from a vector of maybe values.
mkSpec :: (Functor v, Num n) => v (Maybe n) -> SizeSpec v n
mkSpec = dims . fmap (fromMaybe 0)

-- | Make a 'SizeSpec' from a vector.
dims :: Functor v => v n -> SizeSpec v n
dims = SizeSpec

-- | A size spec with no hints to the size.
absolute :: (Additive v, Num n) => SizeSpec v n
absolute = SizeSpec zero

specSize :: (Foldable v, Functor v, Num n, Ord n) => n -> SizeSpec v n -> v n
specSize x (getSpec -> spec) = fmap (fromMaybe smallest) spec
  where
    smallest = fromMaybe x $ minimumOf (folded . _Just) spec

-- | @requiredScale spec sz@ returns the largest scaling factor to make
--   something of size @sz@ fit the requested size @spec@, without changing the
--   aspect ratio. @sz@ should be a strictly positive vector (otherwise a scale
--   of 1 is returned). For non-uniform scaling see 'boxFit'.
requiredScale :: (Additive v, Foldable v, Fractional n, Ord n)
              => SizeSpec v n -> v n -> n
requiredScale (getSpec -> spec) sz
  | F.any (<= 0) sz = 1
  | otherwise       = fromMaybe 1 . minimumOf (folded . _Just)
                    $ liftI2 (^/) spec sz

requiredScaling :: (Additive v, Foldable v, Fractional n, Ord n)
  => SizeSpec v n -> v n -> Transformation v n
requiredScaling spec = scaling . requiredScale spec

-- | Uniformly scale any enveloped object so that it fits within the
--   given size, for non-uniform scaling see 'boxFit'.
sized :: (InSpace v n a, HasLinearMap v, HasBasis v, Transformable a, Enveloped a, Fractional n, Ord n)
      => SizeSpec v n -> a -> a
sized spec a = transform (requiredScaling spec (size a)) a

-- | Uniformly scale an enveloped object so that it \"has the same
--   size as\" (fits within the width and height of) some other
--   object.
sizedAs :: (InSpace v n a, SameSpace a b, HasLinearMap v, HasBasis v, Transformable a,
            Enveloped a, Enveloped b, Fractional n, Ord n)
        => b -> a -> a
sizedAs other = sized (dims $ size other)

getSize :: (Functor v, Foldable v, Num n, Ord n) => n -> SizeSpec v n -> v n
getSize n (getSpec -> spec) = fmap (fromMaybe lower) spec
  where
    lower = fromMaybe n $ minimumOf (folded . _Just) spec


