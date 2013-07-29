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

    ) where

import           Diagrams.Core        (setTrace, trace)
import           Diagrams.Core.Trace

import           Diagrams.Combinators (withTrace)
