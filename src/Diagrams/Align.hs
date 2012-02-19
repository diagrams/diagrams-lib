{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , UndecidableInstances
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Align
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
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
       , alignByDefault

         -- * General alignment functions

       , align
       , center

       ) where

import Graphics.Rendering.Diagrams

import Data.VectorSpace
import Data.AffineSpace (alerp)

import qualified Data.Map as M

-- | Class of things which can be aligned.
class Alignable a where

  -- | @alignBy v d a@ moves the origin of @a@ along the vector
  --   @v@. If @d = 1@, the origin is moved to the edge of the
  --   envelope in the direction of @v@; if @d = -1@, it moves to the
  --   edge of the envelope in the direction of the negation of @v@.
  --   Other values of @d@ interpolate linearly (so for example, @d =
  --   0@ centers the origin along the direction of @v@).
  alignBy :: V a -> Scalar (V a) -> a -> a

-- | Default implementation of 'alignBy' for types with 'HasOrigin'
--   and 'Enveloped' instances.
alignByDefault :: (HasOrigin a, Enveloped a, Num (Scalar (V a)))
               => V a -> Scalar (V a) -> a -> a
alignByDefault v d a = moveOriginTo (alerp (boundary (negateV v) a)
                                    (boundary v a)
                                    ((d + 1) / 2))
                             a

instance (InnerSpace v, OrderedField (Scalar v)) => Alignable (Envelope v) where
  alignBy = alignByDefault

instance (Enveloped b, HasOrigin b) => Alignable [b] where
  alignBy = alignByDefault

instance (Enveloped b, HasOrigin b) => Alignable (M.Map k b) where
  alignBy = alignByDefault

instance ( HasLinearMap v, InnerSpace v, OrderedField (Scalar v)
         , Monoid' m
         ) => Alignable (QDiagram b v m) where
  alignBy = alignByDefault

-- | @align v@ aligns an enveloped object along the edge in the
--   direction of @v@.  That is, it moves the local origin in the
--   direction of @v@ until it is on the edge of the envelope.  (Note
--   that if the local origin is outside the envelope to begin with,
--   it may have to move \"backwards\".)
align :: (Alignable a, Num (Scalar (V a))) => V a -> a -> a
align v = alignBy v 1

-- | @center v@ centers an enveloped object along the direction of
--   @v@.
center :: (Alignable a, Num (Scalar (V a))) => V a -> a -> a
center v = alignBy v 0
