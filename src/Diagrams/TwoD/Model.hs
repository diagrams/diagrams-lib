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
-- import Graphics.Rendering.Diagrams.UDTree
import Diagrams.TwoD.Types
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Util
import Diagrams.Attributes
import Diagrams.Util

import Data.Monoid

import Data.Colour.Names

------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: (Renderable Ellipse b, Backend b R2, Monoid m)
           => AnnDiagram b R2 m -> AnnDiagram b R2 m
showOrigin d = o <> d
  where o     = circle # fc red
                       # lw 0
                       # scale (max (w/50) (h/50))
                       # fmap (const mempty)
        (w,h) = size2D d

-- data OriginOpts b m = OriginOpts { oDia   :: AnnDiagram b R2 m
--                                  , oScale :: Double
--                                  }

-- showOrigin' (OriginOpts o s) d = o' <> d
--   where o' = o # scale (max (w * s) (h * s))
--         (w,h) = size2D d



-- XXX finish:

--   Draw the separating lines between diagrams composed with 'beside'
--   (or any combinators implemented in terms of 'beside', like '(|||)'
--   or '(===)').

-- showSep :: AnnDiagram b R2 m -> AnnDiagram b R2 m
-- showSep d = s <> d
--   where ... = case d of
--                 AnnDiagram (Branch u ds children) -> ...