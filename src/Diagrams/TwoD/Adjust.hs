-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Adjust
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A default diagram-adjustment implementation for two-dimensional
-- diagrams, useful for backend implementors.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Adjust (
  adjustDia2D
  ) where

import Graphics.Rendering.Diagrams

import Diagrams.Attributes  (lw, lc)
import Diagrams.Util        ((#))

import Diagrams.TwoD.Types  (R2)
import Diagrams.TwoD.Util   (size2D, center2D)
import Diagrams.TwoD.Text   (fontSize)

import Data.AffineSpace     ((.-.))

import Data.Colour.Names    (black)
import Data.Monoid          (Monoid)

-- | @adjustDia2D@ provides a useful default implementation of
--   the 'adjustDia' method from the 'Backend' type class.
--
--   As its first argument it requires a method for extracting the
--   requested output size from the rendering options.
--
--   It then performs the following adjustments:
--
--   * Set some default attributes (in case they have not been set):
--
--       * Line width 0.01
--
--       * Line color black
--
--       * Font size 1
--
--   * Freeze the diagram in its final form
--
--   * Scale and translate the diagram to fit within the requested size

adjustDia2D :: Monoid m => (Options b R2 -> R2) -> b -> Options b R2 -> AnnDiagram b R2 m -> AnnDiagram b R2 m
adjustDia2D getSize _ opts d = d # lw 0.01 # lc black # fontSize 1 # freeze
                             # scale s
                             # translate tr
    where (w,h)   = getSize opts
          (wd,hd) = size2D d
          xscale  = w / wd
          yscale  = h / hd
          s       = let s' = min xscale yscale
                    in  if isInfinite s' then 1 else s'
          tr      = (0.5 *. P (w,h)) .-. (s *. center2D d)