{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Trace
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- \"Traces\", aka embedded raytracers, for finding points on the edge
-- of a diagram.  See "Diagrams.Core.Trace" for internal
-- implementation details.
--
-----------------------------------------------------------------------------

module Diagrams.Trace
    ( -- * Types
      Trace, Traced

      -- * Diagram traces
    , trace, setTrace, withTrace

      -- * Querying traces
    , traceV, traceP, maxTraceV, maxTraceP

      -- * Subdiagram traces
    , boundaryFrom, boundaryFromMay

    ) where

import           Diagrams.Core        (HasLinearMap, OrderedField, Point, Subdiagram, location,
                                       origin, setTrace, trace)
import           Diagrams.Core.Trace

import           Data.Maybe
import           Data.Semigroup
import           Data.VectorSpace     (InnerSpace, Scalar, negateV)
import           Diagrams.Combinators (withTrace)

-- | Compute the furthest point on the boundary of a subdiagram,
--   beginning from the location (local origin) of the subdiagram and
--   moving in the direction of the given vector.  If there is no such
--   point, the origin is returned; see also 'boundaryFromMay'.
boundaryFrom :: (HasLinearMap v, OrderedField (Scalar v), InnerSpace v, Semigroup m)
                => Subdiagram b v m -> v -> Point v
boundaryFrom s v = fromMaybe origin $ boundaryFromMay s v

-- | Compute the furthest point on the boundary of a subdiagram,
--   beginning from the location (local origin) of the subdiagram and
--   moving in the direction of the given vector, or @Nothing@ if
--   there is no such point.
boundaryFromMay :: (HasLinearMap v, OrderedField (Scalar v), Semigroup m, InnerSpace v)
                   => Subdiagram b v m -> v -> Maybe (Point v)
boundaryFromMay s v = traceP (location s) (negateV v) s
