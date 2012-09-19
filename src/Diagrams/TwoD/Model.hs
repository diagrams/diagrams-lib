{-# LANGUAGE FlexibleContexts
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Model
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools for visualizing diagrams' internal model: local origins,
-- envelopes, /etc./
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Model
       ( -- * Showing the local origin
         showOrigin
       , showOrigin'
       , OriginOpts(..)
       , showLabels
       ) where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Names

import Diagrams.Path

import Diagrams.TwoD.Types
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Size    (size2D)
import Diagrams.TwoD.Text
import Diagrams.TwoD.Path
import Diagrams.Attributes
import Diagrams.Util

import Control.Arrow (second)
import Data.Semigroup
import Data.Default
import Data.AffineSpace ((.-.))
import Data.VectorSpace ((^*))

import qualified Data.Map as M

import Data.Colour.Names
import Data.Colour (Colour)

------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: (Renderable (Path R2) b, Backend b R2, Monoid' m)
           => QDiagram b R2 m -> QDiagram b R2 m
showOrigin = showOrigin' def

-- | Mark the origin of a diagram, with control over colour and scale
-- of marker dot.
showOrigin' :: (Renderable (Path R2) b, Backend b R2, Monoid' m)
           => OriginOpts -> QDiagram b R2 m -> QDiagram b R2 m
showOrigin' oo d = o <> d
  where o     = stroke (circle sz)
                # fc (oColor oo)
                # lw 0
                # fmap (const mempty)
        (w,h) = size2D d ^* oScale oo
        sz = maximum [w, h, oMinSize oo]

data OriginOpts = OriginOpts { oColor :: Colour Double
                             , oScale :: Double
                             , oMinSize :: Double
                             }

instance Default OriginOpts where
  def = OriginOpts red (1/50) 0.001


------------------------------------------------------------
-- Labeling named points
------------------------------------------------------------

showLabels :: (Renderable Text b, Backend b R2)
           => QDiagram b R2 m -> QDiagram b R2 Any
showLabels d =
             ( mconcat
             . map (\(n,p) -> text (show n) # translate (p .-. origin))
             . concatMap (\(n,ps) -> zip (repeat n) ps)
             . (map . second . map) location
             . M.assocs
             $ m
             ) <>
             fmap (const (Any False)) d
  where
    SubMap m = subMap d
