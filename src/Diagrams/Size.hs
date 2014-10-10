{-# LANGUAGE CPP                  #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable   #-}
#endif
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
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
  ) where

import           Control.Applicative
import           Control.Lens         hiding (transform)
import           Control.Monad
import           Data.Foldable        as F
-- import           Data.Functor.Rep
-- import           Data.Hashable        (Hashable)
import           Data.Maybe
import           Data.Typeable
-- import           GHC.Generics         (Generic)

import           Diagrams.Core
import           Diagrams.Core.Transform

import           Linear.Vector

------------------------------------------------------------
-- Computing diagram sizes
------------------------------------------------------------

-- -- | Compute the size of an enveloped object as a vector.
-- size :: (V a ~ v, N a ~ n, Enveloped a, HasLinearMap v) => a -> v n
-- size d = tabulate $ \(E l) -> diameter (unit l) d

-- | Compute the point in the center of the bounding box of the enveloped object.
-- centerPoint :: (InSpace v n a, Enveloped a, HasLinearMap v, HasBasis v) => a -> Maybe (Point v n)
-- centerPoint = fmap (uncurry $ lerp 0.5) . getCorners . boundingBox

-- | SizeSpec for a
newtype SizeSpec v n = SizeSpec (v n)
  deriving (
#if __GLASGOW_HASKELL__ >= 707
  Typeable,
#endif
  Functor)

#if __GLASGOW_HASKELL__ < 707
instance Typeable2 Path where
  typeOf2 _ = mkTyConApp sizeSpecTyCon []

sizeSpecTyCon :: TyCon
sizeSpecTyCon = mkTyCon3 "diagrams-lib" "Diagrams.Size" "SizeSpec"
#endif

type instance V (SizeSpec v n) = v
type instance N (SizeSpec v n) = n

deriving instance (Show (v n)) => Show (SizeSpec v n)
-- instance Generic (v n)         => Generic (SizeSpec v n)
-- instance Hashable (v n)        => Hashable (SizeSpec v n)

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


-- -- | Adjust the size and position of a 'Diagram' to fit within the requested
-- --   'SizeSpec'.
-- adjustSize
--   :: (V a ~ v, N a ~ n, Enveloped a, Transformable a, Fractional n)
--   => SizeSpec v n          -- ^ Desired size
--   -> a                     -- ^ Object to adjust
--   -> ( (n,n,n)
--      , v n
--      , Transformation v n
--      , QDiagram b v n m)   -- ^
-- adjustSize spec d = ((gToO,nToO,1), s *^ sz, t, transform t d)
--   where
--     s  = requiredScale spec sz
--     sz = size d
--     --
--     sz' = s *^ sz -- size of transformed diagram
-- 
--     -- vector from origin to lower corner of envelope
--     v  = fmap (env . negated) eye
-- 
--     -- transform by moving lower corner to origin and scale
--     t  = scaling s <> translate v
-- 
--     -- gToO = avgScale globalToOutput
--     gToO = s
-- 
--     -- Scaling factor from normalized units to output units: nth root
--     -- of product of diameters along each basis direction.  Note at
--     -- this point the diagram has already had the globalToOutput
--     -- transformation applied, so output = global = local units.
--     -- nToO = product (map (`diameter` d) basis) ** (1 / fromIntegral (dimension d))
--     nToO = productOf folded sz' ** (recip . fromIntegral . dimension) d


-- adjustDiaSize2D szL _ opts d =
--   ( case spec of
--       Dims _ _ -> opts
--       _        -> opts & szL .~ (uncurry Dims . over both (*s) $ sz)
--   , adjustT
--   , d # transform adjustT
--   )
--   where spec = opts ^. szL
--         sz   = size2D d
--         s    = requiredScale spec sz
--         finalSz = case spec of
--                     Dims w h -> (w,h)
--                     _        -> over both (*s) sz
--         tr = (0.5 *. p2 finalSz) .-. (s *. center2D d)
--         adjustT = translation tr <> scaling s

