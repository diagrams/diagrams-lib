{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP                        #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable         #-}
#endif
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Size
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Utilities for working with sizes of objects.
--
-----------------------------------------------------------------------------
module Diagrams.Size
  ( -- * Size spec
    SizeSpec

    -- ** Making size spec
  , mkSizeSpec
  , dims
  , absolute

    -- ** Extracting size specs
  , getSpec
  , specToSize

    -- ** Functions on size specs
  , requiredScale
  , requiredScaling
  , sized
  , sizedAs
  , sizeAdjustment
  ) where

import           Control.Applicative
import           Control.Lens        hiding (transform)
import           Control.Monad
import           Data.Foldable       as F
import           Data.Hashable
import           Data.Semigroup
import           Data.Maybe
import           Data.Typeable
import           GHC.Generics        (Generic)
import           Prelude

import           Diagrams.Core
import           Diagrams.BoundingBox

import           Linear.Affine
import           Linear.Vector

------------------------------------------------------------
-- Computing diagram sizes
------------------------------------------------------------

-- | A 'SizeSpec' is a way of specifying a size without needed lengths for all
--   the dimensions.
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

-- | Retrieve a size spec as a vector of maybe values. Only positive sizes are
--   returned.
getSpec :: (Functor v, Num n, Ord n) => SizeSpec v n -> v (Maybe n)
getSpec (SizeSpec sp) = mfilter (>0) . Just <$> sp

-- | Make a 'SizeSpec' from a vector of maybe values. Any negative values will
--   be ignored. For 2D 'SizeSpec's see 'mkWidth' and 'mkHeight' from
--   "Diagrams.TwoD.Size".
mkSizeSpec :: (Functor v, Num n) => v (Maybe n) -> SizeSpec v n
mkSizeSpec = dims . fmap (fromMaybe 0)

-- | Make a 'SizeSpec' from a vector. Any negative values will be ignored.
dims :: v n -> SizeSpec v n
dims = SizeSpec

-- | A size spec with no hints to the size.
absolute :: (Additive v, Num n) => SizeSpec v n
absolute = SizeSpec zero

-- | @specToSize n spec@ extracts a size from a 'SizeSpec' @sz@. Any values not
--   specified in the spec are replaced by the smallest of the values that are
--   specified. If there are no specified values (i.e. 'absolute') then @n@ is
--   used.
specToSize :: (Foldable v, Functor v, Num n, Ord n) => n -> SizeSpec v n -> v n
specToSize n (getSpec -> spec) = fmap (fromMaybe smallest) spec
  where
    smallest = fromMaybe n $ minimumOf (folded . _Just) spec

-- | @requiredScale spec sz@ returns the largest scaling factor to make
--   something of size @sz@ fit the requested size @spec@ without changing the
--   aspect ratio. @sz@ should be non-zero (otherwise a scale of 1 is
--   returned). For non-uniform scaling see 'boxFit'.
requiredScale :: (Additive v, Foldable v, Fractional n, Ord n)
              => SizeSpec v n -> v n -> n
requiredScale (getSpec -> spec) sz
  | allOf (folded . _Just) (<= 0) usedSz = 1
  | otherwise                            = fromMaybe 1 mScale
  where
    usedSz = liftI2 (<$) sz spec
    scales = liftI2 (^/) spec sz
    mScale = minimumOf (folded . _Just) scales

-- | Return the 'Transformation' calcuated from 'requiredScale'.
requiredScaling :: (Additive v, Foldable v, Fractional n, Ord n)
  => SizeSpec v n -> v n -> Transformation v n
requiredScaling spec = scaling . requiredScale spec

-- | Uniformly scale any enveloped object so that it fits within the
--   given size. For non-uniform scaling see 'boxFit'.
sized :: (InSpace v n a, HasLinearMap v, Transformable a, Enveloped a)
      => SizeSpec v n -> a -> a
sized spec a = transform (requiredScaling spec (size a)) a

-- | Uniformly scale an enveloped object so that it \"has the same
--   size as\" (fits within the width and height of) some other
--   object.
sizedAs :: (InSpace v n a, SameSpace a b, HasLinearMap v, Transformable a,
            Enveloped a, Enveloped b)
        => b -> a -> a
sizedAs other = sized (dims $ size other)

-- | Get the adjustment to fit a 'BoundingBox' in the given 'SizeSpec'. The
--   vector is the new size and the transformation to position the lower
--   corner at the origin and scale to the size spec.
sizeAdjustment :: (Additive v, Foldable v, OrderedField n)
  => SizeSpec v n -> BoundingBox v n -> (v n, Transformation v n)
sizeAdjustment spec bb = (sz', t)
  where
    v = (0.5 *^ P sz') .-. (s *^ fromMaybe origin (boxCenter bb))

    sz  = boxExtents bb
    sz' = if allOf folded isJust (getSpec spec)
            then specToSize 0 spec
            else s *^ sz

    s = requiredScale spec sz

    t = translation v <> scaling s
