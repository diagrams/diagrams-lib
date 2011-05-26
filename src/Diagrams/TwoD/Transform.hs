{-# LANGUAGE FlexibleContexts
           , TypeFamilies
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Transform
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Transformations specific to two dimensions, with a few generic
-- transformations (uniform scaling, translation) also re-exported for
-- convenience.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Transform
       (
         -- * Rotation
         rotation, rotate, rotateBy

         -- * Scaling
       , scalingX, scaleX
       , scalingY, scaleY
       , scaling, scale

       , scaleToX, scaleToY

         -- * Translation
       , translationX, translateX
       , translationY, translateY
       , translation, translate

         -- * Reflection
       , reflectionX, reflectX
       , reflectionY, reflectY
       ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Util

import Control.Arrow (first, second)

-- Rotation ------------------------------------------------

-- | Create a transformation which performs a rotation by the given
--   angle.  See also 'rotate'.
rotation :: Angle a => a -> Transformation R2
rotation ang = fromLinear r (linv r)
  where
    r            = rot theta <-> rot (-theta)
    Rad theta    = convertAngle ang
    rot th (x,y) = (cos th * x - sin th * y, sin th * x + cos th * y)

-- | Rotate by the given angle. Positive angles correspond to
--   counterclockwise rotation, negative to clockwise. The angle can
--   be expressed using any type which is an instance of 'Angle'.  For
--   example, @rotate (1/4)@, @rotate (Rad (pi/2))@, and @rotate (Deg
--   90)@ all represent the same transformation, namely, a
--   counterclockwise rotation by a right angle.
rotate :: (Transformable t, V t ~ R2, Angle a) => a -> t -> t
rotate = transform . rotation

-- XXX add a note about why rotateBy is needed etc.

-- | A synonym for 'rotate', specialized to only work with
--   @CircleFrac@ arguments.
rotateBy :: (Transformable t, V t ~ R2) => CircleFrac -> t -> t
rotateBy = transform . rotation

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the x (horizontal) direction.
scalingX :: Double -> Transformation R2
scalingX c = fromLinear s s
  where s = first (*c) <-> first (/c)

-- | Scale a diagram by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use
--   'Graphics.Rendering.Diagrams.Transform.scale'.
scaleX :: (Transformable t, V t ~ R2) => Double -> t -> t
scaleX = transform . scalingX

-- | Construct a transformation which scales by the given factor in
--   the y (vertical) direction.
scalingY :: Double -> Transformation R2
scalingY c = fromLinear s s
  where s = second (*c) <-> second (/c)

-- | Scale a diagram by the given factor in the y (vertical)
--   direction.  To scale uniformly, use
--   'Graphics.Rendering.Diagrams.Transform.scale'.
scaleY :: (Transformable t, V t ~ R2) => Double -> t -> t
scaleY = transform . scalingY

-- | @scaleToX w@ scales a diagram in the x (horizontal) direction by
--   whatever factor required to make its width @w@.  @scaleToX@
--   should not be applied to diagrams with a width of 0, such as
--   'vrule'.
scaleToX :: (Boundable t, Transformable t, V t ~ R2) => Double -> t -> t
scaleToX w d = scaleX (w / width d) d

-- | @scaleToY h@ scales a diagram in the y (vertical) direction by
--   whatever factor required to make its height @h@.  @scaleToY@
--   should not be applied to diagrams with a width of 0, such as
--   'hrule'.
scaleToY :: (Boundable t, Transformable t, V t ~ R2) => Double -> t -> t
scaleToY h d = scaleY (h / height d) d

-- Translation ---------------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the x (horizontal) direction.
translationX :: Double -> Transformation R2
translationX x = translation (x,0)

-- | Translate a diagram by the given distance in the x (horizontal)
--   direction.
translateX :: (Transformable t, V t ~ R2) => Double -> t -> t
translateX = transform . translationX

-- | Construct a transformation which translates by the given distance
--   in the y (vertical) direction.
translationY :: Double -> Transformation R2
translationY y = translation (0,y)

-- | Translate a diagram by the given distance in the y (vertical)
--   direction.
translateY :: (Transformable t, V t ~ R2) => Double -> t -> t
translateY = transform . translationY

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips a diagram from left to
--   right, i.e. sends the point (x,y) to (-x,y).
reflectionX :: Transformation R2
reflectionX = scalingX (-1)

-- | Flip a diagram from left to right, i.e. send the point (x,y) to
--   (-x,y).
reflectX :: (Transformable t, V t ~ R2) => t -> t
reflectX = transform reflectionX

-- | Construct a transformation which flips a diagram from top to
--   bottom, i.e. sends the point (x,y) to (x,-y).
reflectionY :: Transformation R2
reflectionY = scalingY (-1)

-- | Flip a diagram from top to bottom, i.e. send the point (x,y) to
--   (x,-y).
reflectY :: (Transformable t, V t ~ R2) => t -> t
reflectY = transform reflectionY

-- XXX todo: add general reflection/reflect operators which reflect
-- around an arbitrary axis (taking a vector as an argument);
-- rotations around an arbitrary point; etc.