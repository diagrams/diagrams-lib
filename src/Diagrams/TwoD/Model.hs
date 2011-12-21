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
-- bounding regions, etc.
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

import qualified Data.Map as M

import Data.Colour.Names
import Data.Colour (Colour)

------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: (Renderable (Path R2) b, Backend b R2, Monoid' m)
           => QDiagram b R2 m -> QDiagram b R2 m
showOrigin d = o <> d
  where o     = (stroke $ circle (max (w/50) (h/50)))
                # fc red
                # lw 0
                # fmap (const mempty)
        (w,h) = size2D d

-- | Mark the origin of a diagram, with control over colour and scale
-- of marker dot.
showOrigin' :: (Renderable (Path R2) b, Backend b R2, Monoid' m)
           => OriginOpts -> QDiagram b R2 m -> QDiagram b R2 m
showOrigin' oo d = o <> d
  where o     = (stroke $ circle (max (w * oScale oo) (h * oScale oo)))
                # fc (oColor oo)
                # lw 0
                # fmap (const mempty)
        (w,h) = size2D d

data OriginOpts = OriginOpts { oColor :: Colour Double
                             , oScale :: Double
                             }

instance Default OriginOpts where
  def = OriginOpts red (1/50)


------------------------------------------------------------
-- Labeling named points
------------------------------------------------------------

showLabels :: (Renderable Text b, Backend b R2)
           => QDiagram b R2 m -> QDiagram b R2 Any
showLabels d = (mconcat
             . map (\(n,p) -> text (show n) # translate (p .-. origin))
             . concatMap (\(n,ps) -> zip (repeat n) ps)
             . (map . second . map) location
             . M.assocs
             $ m)
               `atop`
               (fmap (const (Any False)) d)
  where
    NameMap m = names d

-- XXX finish:

--   Draw the separating lines between diagrams composed with 'beside'
--   (or any combinators implemented in terms of 'beside', like '(|||)'
--   or '(===)').

-- showSep :: QDiagram b R2 m -> QDiagram b R2 m
-- showSep d = s <> d
--   where ... = case d of
--                 QDiagram (Branch u ds children) -> ...
