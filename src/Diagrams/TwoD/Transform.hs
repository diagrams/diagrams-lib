{-# LANGUAGE PackageImports, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Transform
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Transformations specific to two dimensions.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Transform
       ( rotation, rotate
       , scalingX, scaleX
       , scalingY, scaleY
       ) where

import "diagrams-core" Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform

import Diagrams.TwoD.Types

import Control.Arrow (first, second)

-- Do we want to rotate things in arbitrary dimensions?

rotation :: Angle -> Transformation P2
rotation theta = fromLinear r (linv r)
  where
    rot th (x,y) = (cos th * x - sin th * y, sin th * x + cos th * y)
    r = rot theta <-> rot (-theta)

rotate :: (TSpace t ~ P2, Transformable t) => Angle -> t -> t
rotate = transform . rotation

scalingX :: Double -> Transformation P2
scalingX c = fromLinear s s
  where s = first (*c) <-> first (/c)

scaleX :: (TSpace t ~ P2, Transformable t) => Double -> t -> t
scaleX = transform . scalingX

scalingY :: Double -> Transformation P2
scalingY c = fromLinear s s
  where s = second (*c) <-> second (/c)

scaleY :: (TSpace t ~ P2, Transformable t) => Double -> t -> t
scaleY = transform . scalingY
