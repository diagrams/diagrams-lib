-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Measured
-- Copyright   :  (c) 2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- 'Measured' diagrams allow access to 'global', 'normalized, 'output'
-- and 'local' units when making a diagram (normal diagrams are in
-- 'local' units). The downside is we can't make use of the 'Envelope'
-- or 'Trace'.
--
-----------------------------------------------------------------------------
module Diagrams.Measured
  (
    -- * Measured diagrams
    MDiagram
  , measuredDiagram

    -- * Internals
  , measuredLeaf
  )
  where

import Data.Semigroup
import Data.Monoid.Coproduct
import Data.Typeable
import Data.Traversable (Traversable)

import Diagrams.Core
import Diagrams.Core.Measure
import Diagrams.Core.Types
import Diagrams.Util

import Linear.Metric

type MDiagram b = Measured b (Diagram b)

-- | Turn a measured diagram into a 'Diagram' with a 'DelayedLeaf'. The
--   resulting diagram has no 'Envelope' or 'Trace'.
--
--   Units are accesable by the 'Functor' instance of 'Measured':
--
-- @
-- outputCircle = fc blue . circle <$> output 10 :: MDiagram B
-- @
--
--  Or the 'Monad' instance:
--
-- @
-- topRight :: Diagram B
-- topRight = measuredDiagram $ do
--   o <- output 10
--   return $ circle o
-- @
--
--  The envelope of the resulting diagram will have no 'Envelope' or
--  'Trace'. You can either get another trace by combining it with
--  another diagram or set the envelope explicitly with the 'envelope'
--  lens.
--
--  The non-nscaling part of transforms are applied as normal, but only
--  'local' units get scaled. Other sizes depend on the final size and
--  output of the diagram.
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
    delayedPrim da g n =
      unmeasure md (l,g,n)
        # transform tr'
        # applyStyle sty
      where
        tr'       = tr <> scaling (1/l)
        (tr, sty) = option mempty untangle . fst $ da
        l         = avgScale tr


