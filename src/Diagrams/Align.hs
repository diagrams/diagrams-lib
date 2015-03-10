{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Align
-- Copyright   :  (c) 2011-2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The /alignment/ of an object refers to the position of its local
-- origin with respect to its envelope.  This module defines the
-- 'Alignable' class for things which can be aligned, as well as a
-- default implementation in terms of 'HasOrigin' and 'Enveloped',
-- along with several utility methods for alignment.
--
-----------------------------------------------------------------------------

module Diagrams.Align
       ( -- * Alignable class

         Alignable(..)
       , alignBy'Default
       , envelopeBoundary
       , traceBoundary

         -- * General alignment functions

       , align
       , snug
       , centerV, center
       , snugBy
       , snugCenterV, snugCenter

       ) where

import           Diagrams.Core
import           Diagrams.Util    (applyAll)

import           Data.Maybe       (fromMaybe)
import           Data.Ord         (comparing)
import           Data.Traversable
import           Prelude

import qualified Data.Foldable    as F
import qualified Data.Map         as M
import qualified Data.Set         as S

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

-- | Class of things which can be aligned.
class Alignable a where

  -- | @alignBy v d a@ moves the origin of @a@ along the vector
  --   @v@. If @d = 1@, the origin is moved to the edge of the
  --   boundary in the direction of @v@; if @d = -1@, it moves to the
  --   edge of the boundary in the direction of the negation of @v@.
  --   Other values of @d@ interpolate linearly (so for example, @d =
  --   0@ centers the origin along the direction of @v@).
  alignBy' :: (InSpace v n a, Fractional n, HasOrigin a)
           => (v n -> a -> Point v n) -> v n -> n -> a -> a
  alignBy' = alignBy'Default

  defaultBoundary :: (V a ~ v, N a ~ n) => v n -> a -> Point v n

  alignBy :: (InSpace v n a, Fractional n, HasOrigin a)
          => v n -> n -> a -> a
  alignBy = alignBy' defaultBoundary

-- | Default implementation of 'alignBy' for types with 'HasOrigin'
--   and 'AdditiveGroup' instances.
alignBy'Default :: (InSpace v n a, Fractional n, HasOrigin a)
                => (v n -> a -> Point v n) -> v n -> n -> a -> a
alignBy'Default boundary v d a = moveOriginTo (lerp ((d + 1) / 2)
                                                    (boundary v a)
                                                    (boundary (negated v) a)
                                              ) a
{-# ANN alignBy'Default ("HLint: ignore Use camelCase" :: String) #-}


-- | Some standard functions which can be used as the `boundary` argument to
--  `alignBy'`.
envelopeBoundary :: (V a ~ v, N a ~ n, Enveloped a) => v n -> a -> Point v n
envelopeBoundary = envelopeP

traceBoundary :: (V a ~ v, N a ~ n, Num n, Traced a) => v n -> a -> Point v n
traceBoundary v a = fromMaybe origin (maxTraceP origin v a)

combineBoundaries
  :: (InSpace v n a, Metric v, Ord n, F.Foldable f)
  => (v n -> a -> Point v n) -> v n -> f a -> Point v n
combineBoundaries b v fa
    = b v $ F.maximumBy (comparing (quadrance . (.-. origin) . b v)) fa

instance (Metric v, OrderedField n) => Alignable (Envelope v n) where
  defaultBoundary = envelopeBoundary

instance (Metric v, OrderedField n) => Alignable (Trace v n) where
  defaultBoundary = traceBoundary

instance (V b ~ v, N b ~ n, Metric v, OrderedField n, Alignable b) => Alignable [b] where
  defaultBoundary = combineBoundaries defaultBoundary

instance (V b ~ v, N b ~ n, Metric v, OrderedField n, Alignable b)
    => Alignable (S.Set b) where
  defaultBoundary = combineBoundaries defaultBoundary

instance (V b ~ v, N b ~ n, Metric v, OrderedField n, Alignable b)
    => Alignable (M.Map k b) where
  defaultBoundary = combineBoundaries defaultBoundary

instance (Metric v, OrderedField n, Monoid' m)
    => Alignable (QDiagram b v n m) where
  defaultBoundary = envelopeBoundary

-- | Although the 'alignBy' method for the @(b -> a)@ instance is
--   sensible, there is no good implementation for
--   'defaultBoundary'. Instead, we provide a total method, but one that
--   is not sensible. This should not present a serious problem as long
--   as your use of 'Alignable' happens through 'alignBy'.
instance (InSpace v n a, HasOrigin a, Alignable a) => Alignable (b -> a) where
  alignBy v d f b     = alignBy v d (f b)
  defaultBoundary _ _ = origin

-- | @align v@ aligns an enveloped object along the edge in the
--   direction of @v@.  That is, it moves the local origin in the
--   direction of @v@ until it is on the edge of the envelope.  (Note
--   that if the local origin is outside the envelope to begin with,
--   it may have to move \"backwards\".)
align :: (InSpace v n a, Fractional n, Alignable a, HasOrigin a) => v n -> a -> a
align v = alignBy v 1

-- | Version of @alignBy@ specialized to use @traceBoundary@
snugBy :: (InSpace v n a, Fractional n, Alignable a, Traced a, HasOrigin a)
       => v n -> n -> a -> a
snugBy = alignBy' traceBoundary

-- | Like align but uses trace.
snug :: (InSpace v n a, Fractional n, Alignable a, Traced a, HasOrigin a)
      => v n -> a -> a
snug v = snugBy v 1

-- | @centerV v@ centers an enveloped object along the direction of
--   @v@.
centerV :: (InSpace v n a, Fractional n, Alignable a, HasOrigin a) => v n -> a -> a
centerV v = alignBy v 0

-- | @center@ centers an enveloped object along all of its basis vectors.
center :: (InSpace v n a, Fractional n, Traversable v, Alignable a, HasOrigin a) => a -> a
center = applyAll fs
  where
    fs = map centerV basis

-- | Like @centerV@ using trace.
snugCenterV
  :: (InSpace v n a, Fractional n, Alignable a, Traced a, HasOrigin a)
   => v n -> a -> a
snugCenterV v = alignBy' traceBoundary v 0

-- | Like @center@ using trace.
snugCenter :: (InSpace v n a, Traversable v, Fractional n, Alignable a, HasOrigin a, Traced a)
           => a -> a
snugCenter = applyAll fs
  where
    fs = map snugCenterV basis

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
