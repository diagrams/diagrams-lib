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

import Data.Monoid

-- | Place two diagrams next to each other along the given vector.
--   XXX write more. note where origin ends up.
beside :: ( Backend b
          , v ~ BSpace b
          , Transformable v
          , v ~ TSpace v
          , HasLinearMap v
          , HasLinearMap (Scalar v)
          , InnerSpace v
          , AdditiveGroup (Scalar v)
          , Fractional (Scalar v)
          , Ord (Scalar v)
          , Scalar (Scalar v) ~ Scalar v
          , Monoid a
          )
       => v -> AnnDiagram b a -> AnnDiagram b a -> AnnDiagram b a
beside v d1@(Diagram _ (Bounds b1) _ _)
         d2@(Diagram _ (Bounds b2) _ _)
  = rebase (P $ b1 v *^ v) d1 `atop`
    rebase (P $ b2 (negateV v) *^ negateV v) d2
