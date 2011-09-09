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
  , adjustSize
  ) where

import Graphics.Rendering.Diagrams

import Diagrams.Attributes  (lw, lc)
import Diagrams.Util        ((#))

import Diagrams.TwoD.Types  (R2)
import Diagrams.TwoD.Size   (size2D, center2D, SizeSpec2D(..))
import Diagrams.TwoD.Text   (fontSize)

import Data.AffineSpace     ((.-.))

import Data.Colour.Names    (black)
import Data.Monoid          (Monoid, mempty)

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
          s'      = min xscale yscale
          s | isInfinite s' = 1
            | otherwise     = s'
          tr      = (0.5 *. P (w,h)) .-. (s *. center2D d)

-- | @adjustSize spec sz@ returns a transformation which can be
--   applied to something of size @sz@ to make it the requested size
--   @spec@.
adjustSize :: SizeSpec2D -> R2 -> Transformation R2
adjustSize Absolute _ = mempty
adjustSize (Width wSpec) (w,_)
  | wSpec == 0 || w == 0 = mempty
  | otherwise = scaling (wSpec / w)
adjustSize (Height hSpec) (_,h)
  | hSpec == 0 || h == 0 = mempty
  | otherwise = scaling (hSpec / h)
adjustSize (Dims wSpec hSpec) (w,h) = scaling s
  where xscale  = wSpec / w
        yscale  = hSpec / h
        s'      = min xscale yscale
        s | isInfinite s' = 1
          | otherwise     = s'
