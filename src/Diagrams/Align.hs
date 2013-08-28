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
       , center

       ) where

import           Diagrams.Core

import           Data.AffineSpace (alerp, (.-.))
import           Data.VectorSpace
import           Data.Maybe       (fromMaybe)
import           Data.Ord         (comparing)

import qualified Data.Map         as M
import qualified Data.Set         as S
import qualified Data.Foldable    as F

-- | Class of things which can be aligned.
class Alignable a where

  -- | @alignBy v d a@ moves the origin of @a@ along the vector
  --   @v@. If @d = 1@, the origin is moved to the edge of the
  --   boundary in the direction of @v@; if @d = -1@, it moves to the
  --   edge of the boundary in the direction of the negation of @v@.
  --   Other values of @d@ interpolate linearly (so for example, @d =
  --   0@ centers the origin along the direction of @v@).
  alignBy' :: ( HasOrigin a, AdditiveGroup (V a), Num (Scalar (V a))
              , Fractional (Scalar (V a)))
            => (V a -> a -> Point (V a)) -> V a -> Scalar (V a) -> a -> a
  alignBy' = alignBy'Default

  defaultBoundary :: V a -> a -> Point (V a)

  alignBy :: (HasOrigin a, Num (Scalar (V a)), Fractional (Scalar (V a)))
           => V a -> Scalar (V a) -> a -> a
  alignBy = alignBy' defaultBoundary

-- | Default implementation of 'alignBy' for types with 'HasOrigin'
--   and 'Enveloped' instances.
alignBy'Default :: ( HasOrigin a, AdditiveGroup (V a), Num (Scalar (V a))
                   , Fractional (Scalar (V a)))
                 => (V a -> a -> Point (V a)) -> V a -> Scalar (V a) -> a -> a
alignBy'Default boundary v d a = moveOriginTo (alerp (boundary (negateV v) a)
                                    (boundary v a)
                                    ((d + 1) / 2)) a

envelopeBoundary :: Enveloped a => V a -> a -> Point (V a)
envelopeBoundary = envelopeP

traceBoundary :: Traced a => V a -> a -> Point (V a)
traceBoundary v a = fromMaybe origin (maxTraceP origin v a)

combineBoundaries
  :: (F.Foldable f, InnerSpace (V a), Ord (Scalar (V a)))
  => (V a -> a -> Point (V a)) -> (V a -> f a -> Point (V a))
combineBoundaries b v fa
    = b v $ F.maximumBy (comparing (magnitudeSq . (.-. origin) . b v)) fa
--combineBoundaries b v = F.maximumBy (comparing (magnitudeSq . (.-. origin) . b v))

instance (InnerSpace v, OrderedField (Scalar v)) => Alignable (Envelope v) where
  defaultBoundary = envelopeBoundary

instance (InnerSpace v, OrderedField (Scalar v)) => Alignable (Trace v) where
  defaultBoundary = traceBoundary

instance (InnerSpace (V b), Ord (Scalar (V b)), Alignable b)
       => Alignable [b] where
  defaultBoundary = combineBoundaries defaultBoundary

instance (InnerSpace (V b), Ord (Scalar (V b)), Alignable b)
       => Alignable (S.Set b) where
  defaultBoundary = combineBoundaries defaultBoundary

instance (InnerSpace (V b), Ord (Scalar (V b)), Alignable b)
       => Alignable (M.Map k b) where
  defaultBoundary = combineBoundaries defaultBoundary

instance ( HasLinearMap v, InnerSpace v, OrderedField (Scalar v)
         , Monoid' m
         ) => Alignable (QDiagram b v m) where
  defaultBoundary = envelopeBoundary

-- | @align v@ aligns an bounded object along the edge in the
--   direction of @v@.  That is, it moves the local origin in the
--   direction of @v@ until it is on the edge of the boundary.  (Note
--   that if the local origin is outside the boundary to begin with,
--   it may have to move \"backwards\".)
align :: ( Alignable a, HasOrigin a, Num (Scalar (V a))
         , Fractional (Scalar (V a))) => V a -> a -> a
align v = alignBy v 1

-- | @center v@ centers an bounded object along the direction of
--   @v@.
center :: ( Alignable a, HasOrigin a, Num (Scalar (V a))
          , Fractional (Scalar (V a))) => V a -> a -> a
center v = alignBy v 0

