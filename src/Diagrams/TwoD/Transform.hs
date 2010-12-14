{-# LANGUAGE TypeFamilies #-}
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
       , translationX, translateX
       , translationY, translateY
       , reflectionX, reflectX
       , reflectionY, reflectY
       ) where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform

import Diagrams.TwoD.Types

import Control.Arrow (first, second)

-- Do we want to rotate things in arbitrary dimensions?

rotation :: Angle -> Transformation R2
rotation theta = fromLinear r (linv r)
  where
    rot th (x,y) = (cos th * x - sin th * y, sin th * x + cos th * y)
    r = rot theta <-> rot (-theta)

rotate :: (TSpace t ~ R2, Transformable t) => Angle -> t -> t
rotate = transform . rotation

-- | Construct a transformation which scales by the given factor in
--   the x (horizontal) direction.
scalingX :: Double -> Transformation R2
scalingX c = fromLinear s s
  where s = first (*c) <-> first (/c)

-- | Scale a diagram by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use
--   'Graphics.Rendering.Diagrams.Transform.scale'.
scaleX :: (TSpace t ~ R2, Transformable t) => Double -> t -> t
scaleX = transform . scalingX

-- | Construct a transformation which scales by the given factor in
--   the y (vertical) direction.
scalingY :: Double -> Transformation R2
scalingY c = fromLinear s s
  where s = second (*c) <-> second (/c)

-- | Scale a diagram by the given factor in the y (vertical)
--   direction.  To scale uniformly, use
--   'Graphics.Rendering.Diagrams.Transform.scale'.
scaleY :: (TSpace t ~ R2, Transformable t) => Double -> t -> t
scaleY = transform . scalingY

translationX :: Double -> Transformation R2
translationX x = translation (x,0)

translateX :: (TSpace t ~ R2, Transformable t) => Double -> t -> t
translateX = transform . translationX

translationY :: Double -> Transformation R2
translationY y = translation (0,y)

translateY :: (TSpace t ~ R2, Transformable t) => Double -> t -> t
translateY = transform . translationY

reflectionX :: Transformation R2
reflectionX = scalingX (-1)

reflectX :: (TSpace t ~ R2, Transformable t) => t -> t
reflectX = transform reflectionX

reflectionY :: Transformation R2
reflectionY = scalingY (-1)

reflectY :: (TSpace t ~ R2, Transformable t) => t -> t
reflectY = transform reflectionY