-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Envelope
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- \"Envelopes\", aka functional bounding regions.  See
-- "Diagrams.Core.Envelope" for internal implementation details.
--
-----------------------------------------------------------------------------

module Diagrams.Envelope
    ( -- * Types
      Envelope, Enveloped

      -- * Diagram envelopes
    , envelope, setEnvelope

      -- * Querying envelopes
    , envelopeVMay, envelopeV, envelopePMay, envelopeP
    , diameter, radius

    ) where

import           Diagrams.Core          (envelope, setEnvelope)
import           Diagrams.Core.Envelope
