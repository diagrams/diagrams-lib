module Diagrams.Measured where

import Data.Functor
import Data.Semigroup
import Data.Monoid.Coproduct
import Data.Typeable
import Data.Traversable (Traversable)

import Diagrams.Core
import Diagrams.Core.Measure
import Diagrams.Core.Types
import Diagrams.Util

import Linear.Metric

-- | Turn a measured diagram into a 'Diagram' with a 'DelayedLeaf'. The
--   resulting diagram has no 'Envelope' or 'Trace and only 'local' units are
--   scaled.
measuredDiagram :: (Metric v, Traversable v, OrderedField n, Typeable n, Monoid' m)
  => Measured n (QDiagram b v n m) -> QDiagram b v n m
measuredDiagram md
  = mkQD' (measuredLeaf md)
          mempty -- envelope
          mempty -- trace
          mempty -- submap
          mempty -- query

-- | Turn a measured diagram into a 'DelayedLeaf'.
measuredLeaf :: (Metric v, Traversable v, OrderedField n, Typeable n, Monoid' m)
  => Measured n (QDiagram b v n m) -> QDiaLeaf b v n m
measuredLeaf md = DelayedLeaf delayedPrim
  where
    delayedPrim da g n = unmeasure md (l,g,n)
                           # transform tr'
                           # applyStyle sty
      where
        tr'       = tr <> scaling (1/l)
        (tr, sty) = option mempty untangle . fst $ da
        l         = avgScale tr

