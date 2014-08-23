{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
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

import           Control.Lens             (makeLenses, (^.))

import           Diagrams.Core
import           Diagrams.Path
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Ellipse
import           Diagrams.TwoD.Path
import           Diagrams.TwoD.Size       (size2D)
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Types
import           Diagrams.Util

import           Control.Arrow            (second)
import           Data.Default.Class
import           Data.Semigroup

import qualified Data.Map                 as M

import           Data.Colour              (Colour)
import           Data.Colour.Names

import Linear.Affine
import Linear.Vector
import Data.Data

------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------

data OriginOpts n = OriginOpts
  { _oColor   :: Colour Double
  , _oScale   :: n
  , _oMinSize :: n
  }

makeLenses ''OriginOpts

instance Fractional n => Default (OriginOpts n) where
  def = OriginOpts red (1/50) 0.001

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: (RealFloat n, OrderedField n, Renderable (Path V2 n) b, Data n, Monoid' m)
           => QDiagram b V2 n m -> QDiagram b V2 n m
showOrigin = showOrigin' def

-- | Mark the origin of a diagram, with control over colour and scale
-- of marker dot.
showOrigin' :: (RealFloat n, OrderedField n, Renderable (Path V2 n) b, Data n, Monoid' m)
           => OriginOpts n -> QDiagram b V2 n m -> QDiagram b V2 n m
showOrigin' oo d = o <> d
  where o     = stroke (circle sz)
                # fc (oo^.oColor)
                # lineWidth (Output 0)
                # fmap (const mempty)
        (w,h) = size2D d ^* oo^.oScale
        sz    = maximum [w, h, oo^.oMinSize]

------------------------------------------------------------
-- Labeling named points
------------------------------------------------------------

showLabels :: (Typeable n, OrderedField n, Renderable (Text n) b, Semigroup m)
           => QDiagram b V2 n m -> QDiagram b V2 n Any
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
