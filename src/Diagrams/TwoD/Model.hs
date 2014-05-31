{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell, ConstraintKinds, TypeFamilies       #-}
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
       , OriginOpts(..), oColor, oScale, oMinSize
       , showLabels
       ) where

import           Control.Lens          (makeLenses, (^.))

import           Diagrams.Core
import           Diagrams.TwoD.Attributes
import           Diagrams.Path
import           Diagrams.TwoD.Ellipse
import           Diagrams.TwoD.Path
import           Diagrams.TwoD.Size    (size2D)
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Types
import           Diagrams.Util

import           Control.Arrow         (second)
import           Data.AffineSpace      ((.-.))
import           Data.Default.Class
import           Data.Semigroup
import           Data.VectorSpace      ((^*),Scalar)

import qualified Data.Map              as M

import           Data.Colour           (Colour)
import           Data.Colour.Names

------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------

data OriginOpts d = OriginOpts { _oColor   :: Colour Double
                             , _oScale   :: d
                             , _oMinSize :: d
                             }

makeLenses ''OriginOpts

instance (Fractional d) => Default (OriginOpts d) where
  def = OriginOpts red (1/50) 0.001

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: (Renderable (Path v) b, R2Ish v, Backend b v, Monoid' m)
           => QDiagram b v m -> QDiagram b v m
showOrigin = showOrigin' def

-- | Mark the origin of a diagram, with control over colour and scale
-- of marker dot.
showOrigin' :: (Renderable (Path v) b, R2Ish v, Backend b v, Monoid' m)
           => OriginOpts (Scalar v) -> QDiagram b v m -> QDiagram b v m
showOrigin' oo d = o <> d
  where o     = stroke (circle sz)
                # fc (oo^.oColor)
                # lineWidth (Output 0)
                # fmap (const mempty)
        (w,h) = size2D d ^* oo^.oScale
        sz = maximum [w, h, oo^.oMinSize]

------------------------------------------------------------
-- Labeling named points
------------------------------------------------------------

showLabels :: (Renderable (Text v) b, R2Ish v, Backend b v, Semigroup m)
           => QDiagram b v m -> QDiagram b v Any
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
    SubMap m = d^.subMap
