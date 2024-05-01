{-# LANGUAGE CPP #-}

-- |
-- Module      :  Linear.Vector.Compat
-- Copyright   :  (c) 2024 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Compatibility layer for working with versions of linear both before
-- (< 1.23) and after (>= 1.23) the interpolation direction of `lerp`
-- was reversed.
module Linear.Vector.Compat (lerp) where

import qualified Linear.Vector as V

-- | Linearly interpolate between two vectors, such that @lerp 0 x y =
--   x@ and @lerp 1 x y = 1@.
lerp :: (V.Additive f, Num a) => a -> f a -> f a -> f a
lerp =
#if MIN_VERSION_linear(1,23,0)
  V.lerp
#else
  V.lerp . (1-)
#endif
