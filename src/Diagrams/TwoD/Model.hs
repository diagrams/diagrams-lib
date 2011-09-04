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
       , showLabels
       ) where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Names

import Diagrams.TwoD.Types
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Util
import Diagrams.TwoD.Text
import Diagrams.Attributes
import Diagrams.Util

import Control.Arrow (second)
import Data.Monoid
import Data.AffineSpace ((.-.))

import qualified Data.Map as M

import Data.Colour.Names

------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: (Renderable Ellipse b, Backend b R2, Monoid m)
           => AnnDiagram b R2 m -> AnnDiagram b R2 m
showOrigin d = o <> d
  where o     = circle (max (w/50) (h/50))
                # fc red
                # lw 0
                # fmap (const mempty)
        (w,h) = size2D d

-- data OriginOpts b m = OriginOpts { oDia   :: AnnDiagram b R2 m
--                                  , oScale :: Double
--                                  }

-- showOrigin' (OriginOpts o s) d = o' <> d
--   where o' = o # scale (max (w * s) (h * s))
--         (w,h) = size2D d


------------------------------------------------------------
-- Labeling named points
------------------------------------------------------------

showLabels :: (Renderable Text b, Backend b R2)
           => AnnDiagram b R2 m -> AnnDiagram b R2 Any
showLabels d = (mconcat
             . map (\(n,p) -> text (show n) # translate (p .-. origin))
             . concatMap (\(n,ps) -> zip (repeat n) ps)
             . (map . second . map) fst
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

-- showSep :: AnnDiagram b R2 m -> AnnDiagram b R2 m
-- showSep d = s <> d
--   where ... = case d of
--                 AnnDiagram (Branch u ds children) -> ...