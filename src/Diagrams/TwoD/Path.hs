{-# LANGUAGE FlexibleContexts
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Paths in two dimensions are special since we may stroke them to
-- create a 2D diagram, and (eventually) perform operations such as
-- intersection and union.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Path
       ( -- * Constructing path-based diagrams

         stroke, strokeT

       ) where

import Graphics.Rendering.Diagrams
import Diagrams.Path

import Data.VectorSpace

import Data.Monoid

------------------------------------------------------------
--  Constructing path-based diagrams  ----------------------
------------------------------------------------------------

-- | Convert a path into a diagram.  The resulting diagram has the
--   names 0, 1, ... assigned to each of the path's vertices.
--
--   Note that a bug in GHC 7.0.1 causes a context stack overflow when
--   inferring the type of @stroke@.  The solution is to give a type
--   signature to expressions involving @stroke@, or (recommended)
--   upgrade GHC (the bug is fixed in 7.0.2 onwards).
stroke :: (InnerSpace v, OrderedField (Scalar v), Renderable (Path v) b)
       => Path v -> Diagram b v
stroke p = mkAD (Prim p)
                (getBounds p)
                mempty
                {-  XXX what to do here?
                    fromNames $ zip ([0..] :: [Int])
                                    (pathVertices p)  -- XXX names for Bezier
                                                      --   control points too?
                -}
                mempty   -- Paths are infinitely thin
                         -- TODO: what about closed paths in 2D?

-- | A composition of 'stroke' and 'pathFromTrail' for conveniently
--   converting a trail directly into a diagram.
--
--   Note that a bug in GHC 7.0.1 causes a context stack overflow when
--   inferring the type of 'stroke' and hence of @strokeT@ as well.
--   The solution is to give a type signature to expressions involving
--   @strokeT@, or (recommended) upgrade GHC (the bug is fixed in 7.0.2
--   onwards).
strokeT :: (InnerSpace v, OrderedField (Scalar v), Renderable (Path v) b)
        => Trail v -> Diagram b v
strokeT = stroke . pathFromTrail
