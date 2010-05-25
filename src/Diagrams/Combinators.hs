{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Combinators
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Higher-level tools for combining diagrams.
--
-----------------------------------------------------------------------------

module Diagrams.Combinators where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform (HasLinearMap)

import Data.VectorSpace

-- | Place two diagrams next to each other along the given vector.
--   XXX write more. note where origin ends up.
beside :: ( Backend b
          , v ~ BSpace b
          , HasLinearMap v
          , HasLinearMap (Scalar v)
          , InnerSpace v
          , AdditiveGroup (Scalar v)
          , Fractional (Scalar v)
          , Ord (Scalar v)
          , Scalar (Scalar v) ~ Scalar v)
       => v -> Diagram b -> Diagram b -> Diagram b
beside v d1@(Diagram _ (Bounds b1) _)
         d2@(Diagram _ (Bounds b2) _)
  = rebase (Const (b1 v *^ v)) d1 `atop`
    rebase (Const (b2 (negateV v) *^ negateV v)) d2
