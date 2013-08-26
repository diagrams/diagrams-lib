{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types           #-}


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
       , alignByDefault

         -- * General alignment functions

       , align'
       , align
       , center'
       , center

       , AlignOpts(..)
       , traceOriginP

       ) where

import           Diagrams.Core

import           Data.Default.Class
import           Data.AffineSpace   (alerp)
import           Data.VectorSpace
import           Data.Maybe         (fromMaybe)


import qualified Data.Map           as M
import qualified Data.Set           as S

data AlignOpts a = AlignOpts { boundary :: V a -> a -> Point (V a) }

instance Enveloped a => Default (AlignOpts a) where
  def = AlignOpts { boundary=envelopeP }

traceOriginP :: (HasOrigin a, Traced a) => V a -> a -> Point (V a)
traceOriginP v a = fromMaybe origin (traceP origin (negateV v) a)

-- | Class of things which can be aligned.
class Alignable a where

  -- | @alignBy' v d a@ moves the origin of @a@ along the vector
  --   @v@. If @d = 1@, the origin is moved to the edge of the boundary
  --   (envelope or trace), depending on AlignOpts, in the direction of @v@;
  --   if @d = -1@, it moves to the edge of the boundary in the
  --   direction of the negation of @v@. Other values of @d@ interpolate
  --   linearly (so for example, @d = 0@ centers the origin along the direction
  --   of @v@).
  alignBy' :: AlignOpts a -> V a -> Scalar (V a) -> a -> a

  alignBy :: V a -> Scalar (V a) -> a -> a

-- | Default implementation of 'alignBy' for types with 'HasOrigin', 'Traced',
--   and 'Enveloped' instances.
alignByDefault :: (HasOrigin a, Enveloped a, Traced a, Num (Scalar (V a)))
               => AlignOpts a -> V a -> Scalar (V a) -> a -> a
alignByDefault opts v d a = moveOriginTo (alerp (boundryP (negateV v) a)
                                    (boundryP v a)
                                    ((d + 1) / 2)) a
  where boundryP = (boundary opts)

instance (InnerSpace v, OrderedField (Scalar v))
       => Alignable (Envelope v) where
  alignBy = alignByDefault def
  alignBy' opts = alignByDefault opts

instance (InnerSpace v, OrderedField (Scalar v))
       => Alignable (Trace v) where
  alignBy = alignByDefault def
  alignBy' opts = alignByDefault opts

instance (Enveloped b, Traced b, HasOrigin b) => Alignable [b] where
  alignBy = alignByDefault def
  alignBy' opts = alignByDefault opts

instance (Enveloped b, Traced b, HasOrigin b, Ord b) => Alignable (S.Set b) where
  alignBy = alignByDefault def
  alignBy' opts = alignByDefault opts

instance (Enveloped b, Traced b, HasOrigin b) => Alignable (M.Map k b) where
  alignBy = alignByDefault def
  alignBy' opts = alignByDefault opts

instance ( HasLinearMap v, InnerSpace v, OrderedField (Scalar v)
         , Monoid' m
         ) => Alignable (QDiagram b v m) where
  alignBy = alignByDefault def
  alignBy' opts = alignByDefault opts

-- | @align' v@ aligns an  object along the edge in the
--   direction of @v@.  That is, it moves the local origin in the
--   direction of @v@ until it is on the edge of the boundary.  (Note
--   that if the local origin is outside the boundary to begin with,
--   it may have to move \"backwards\".)
align' :: (Alignable a, Num (Scalar (V a))) => AlignOpts a -> V a -> a -> a
align' opts v = alignBy' opts v 1

align :: (Alignable a, Num (Scalar (V a))) => V a -> a -> a
align v = alignBy v 1

-- | @center v@ centers an object along the direction of @v@.
center' :: (Alignable a, Num (Scalar (V a))) => AlignOpts a -> V a -> a -> a
center' opts v = alignBy' opts v 0

center :: (Alignable a, Num (Scalar (V a))) => V a -> a -> a
center v = alignBy v 0
