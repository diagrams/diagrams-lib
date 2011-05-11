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
       ) where

import Graphics.Rendering.Diagrams
import Diagrams.TwoD.Types
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Util
import Diagrams.Attributes
import Diagrams.Util

import Data.Colour.Names

------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: (Renderable Ellipse b, Backend b R2)
           => Diagram b R2 -> Diagram b R2
showOrigin d = o <> d
  where o     = circle # fc red
                       # lw 0
                       # scale (max (w/50) (h/50))
        (w,h) = size2D d

-- data OriginOpts b m = OriginOpts { oDia   :: AnnDiagram b R2 m
--                                  , oScale :: Double
--                                  }

-- showOrigin' (OriginOpts o s) d = o' <> d
--   where o' = o # scale (max (w * s) (h * s))
--         (w,h) = size2D d