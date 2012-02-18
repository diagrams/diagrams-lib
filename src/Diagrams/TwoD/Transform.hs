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

       , rotationAbout, rotateAbout

         -- * Scaling
       , scalingX, scaleX
       , scalingY, scaleY
       , scaling, scale

       , scaleToX, scaleToY
       , scaleUToX, scaleUToY

         -- * Translation
       , translationX, translateX
       , translationY, translateY
       , translation, translate

         -- * Reflection
       , reflectionX, reflectX
       , reflectionY, reflectY
       , reflectionAbout, reflectAbout

         -- * Shears
       , shearingX, shearX
       , shearingY, shearY

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.TwoD.Types
import Diagrams.TwoD.Size   (width, height)
import Diagrams.TwoD.Vector (direction)
import Diagrams.Transform

import Data.Semigroup

import Data.AffineSpace

import Control.Arrow (first, second)

-- Rotation ------------------------------------------------

-- | Create a transformation which performs a rotation by the given
--   angle.  See also 'rotate'.
rotation :: Angle a => a -> T2
rotation ang = fromLinear r (linv r)
  where
    r            = rot theta <-> rot (-theta)
    Rad theta    = convertAngle ang
    rot th (x,y) = (cos th * x - sin th * y, sin th * x + cos th * y)

-- | Rotate by the given angle. Positive angles correspond to
--   counterclockwise rotation, negative to clockwise. The angle can
--   be expressed using any type which is an instance of 'Angle'.  For
--   example, @rotate (1\/4 :: 'CircleFrac')@, @rotate (tau\/4 :: 'Rad')@, and
--   @rotate (90 :: 'Deg')@ all represent the same transformation, namely,
--   a counterclockwise rotation by a right angle.
--
--   Note that writing @rotate (1\/4)@, with no type annotation, will
--   yield an error since GHC cannot figure out which sort of angle
--   you want to use.  In this common situation you can use
--   'rotateBy', which is specialized to take a 'CircleFrac' argument.
rotate :: (Transformable t, V t ~ R2, Angle a) => a -> t -> t
rotate = transform . rotation

-- | A synonym for 'rotate', specialized to only work with
--   @CircleFrac@ arguments; it can be more convenient to write
--   @rotateBy (1\/4)@ than @'rotate' (1\/4 :: 'CircleFrac')@.
rotateBy :: (Transformable t, V t ~ R2) => CircleFrac -> t -> t
rotateBy = transform . rotation

-- | @rotationAbout p@ is a rotation about the point @p@ (instead of
--   around the local origin).
rotationAbout :: Angle a => P2 -> a -> T2
rotationAbout p angle = conjugate (translation (origin .-. p)) (rotation angle)

-- | @rotateAbout p@ is like 'rotate', except it rotates around the
--   point @p@ instead of around the local origin.
rotateAbout :: (Transformable t, V t ~ R2, Angle a) => P2 -> a -> t -> t
rotateAbout p angle = rotate angle `under` translation (origin .-. p)

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the x (horizontal) direction.
scalingX :: Double -> T2
scalingX c = fromLinear s s
  where s = first (*c) <-> first (/c)

-- | Scale a diagram by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use 'scale'.
scaleX :: (Transformable t, V t ~ R2) => Double -> t -> t
scaleX = transform . scalingX

-- | Construct a transformation which scales by the given factor in
--   the y (vertical) direction.
scalingY :: Double -> T2
scalingY c = fromLinear s s
  where s = second (*c) <-> second (/c)

-- | Scale a diagram by the given factor in the y (vertical)
--   direction.  To scale uniformly, use 'scale'.
scaleY :: (Transformable t, V t ~ R2) => Double -> t -> t
scaleY = transform . scalingY

-- | @scaleToX w@ scales a diagram in the x (horizontal) direction by
--   whatever factor required to make its width @w@.  @scaleToX@
--   should not be applied to diagrams with a width of 0, such as
--   'vrule'.
scaleToX :: (Enveloped t, Transformable t, V t ~ R2) => Double -> t -> t
scaleToX w d = scaleX (w / width d) d

-- | @scaleToY h@ scales a diagram in the y (vertical) direction by
--   whatever factor required to make its height @h@.  @scaleToY@
--   should not be applied to diagrams with a height of 0, such as
--   'hrule'.
scaleToY :: (Enveloped t, Transformable t, V t ~ R2) => Double -> t -> t
scaleToY h d = scaleY (h / height d) d

-- | @scaleUToX w@ scales a diagram /uniformly/ by whatever factor
--   required to make its width @w@.  @scaleUToX@ should not be
--   applied to diagrams with a width of 0, such as 'vrule'.
scaleUToX :: (Enveloped t, Transformable t, V t ~ R2) => Double -> t -> t
scaleUToX w d = scale (w / width d) d

-- | @scaleUToY h@ scales a diagram /uniformly/ by whatever factor
--   required to make its height @h@.  @scaleUToY@ should not be applied
--   to diagrams with a height of 0, such as 'hrule'.
scaleUToY :: (Enveloped t, Transformable t, V t ~ R2) => Double -> t -> t
scaleUToY h d = scale (h / height d) d

-- Translation ---------------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the x (horizontal) direction.
translationX :: Double -> T2
translationX x = translation (x,0)

-- | Translate a diagram by the given distance in the x (horizontal)
--   direction.
translateX :: (Transformable t, V t ~ R2) => Double -> t -> t
translateX = transform . translationX

-- | Construct a transformation which translates by the given distance
--   in the y (vertical) direction.
translationY :: Double -> T2
translationY y = translation (0,y)

-- | Translate a diagram by the given distance in the y (vertical)
--   direction.
translateY :: (Transformable t, V t ~ R2) => Double -> t -> t
translateY = transform . translationY

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips a diagram from left to
--   right, i.e. sends the point (x,y) to (-x,y).
reflectionX :: T2
reflectionX = scalingX (-1)

-- | Flip a diagram from left to right, i.e. send the point (x,y) to
--   (-x,y).
reflectX :: (Transformable t, V t ~ R2) => t -> t
reflectX = transform reflectionX

-- | Construct a transformation which flips a diagram from top to
--   bottom, i.e. sends the point (x,y) to (x,-y).
reflectionY :: T2
reflectionY = scalingY (-1)

-- | Flip a diagram from top to bottom, i.e. send the point (x,y) to
--   (x,-y).
reflectY :: (Transformable t, V t ~ R2) => t -> t
reflectY = transform reflectionY

-- | @reflectionAbout p v@ is a reflection in the line determined by
--   the point @p@ and vector @v@.
reflectionAbout :: P2 -> R2 -> T2
reflectionAbout p v =
  conjugate (rotation (-direction v :: Rad) <> translation (origin .-. p))
            reflectionY

-- | @reflectAbout p v@ reflects a diagram in the line determined by
--   the point @p@ and the vector @v@.
reflectAbout :: (Transformable t, V t ~ R2) => P2 -> R2 -> t -> t
reflectAbout p v = transform (reflectionAbout p v)

-- Shears --------------------------------------------------

-- | @shearingX d@ is the linear transformation which is the identity on
--   y coordinates and sends @(0,1)@ to @(d,1)@.
shearingX :: Double -> T2
shearingX d = fromLinear (sh d <-> sh (-d)) (sh' d <-> sh' (-d))
  where sh  k (x, y) = (x+k*y, y)
        sh' k        = swap . sh k . swap
        swap (x,y) = (y,x)

-- | @shearX d@ performs a shear in the x-direction which sends
--   @(0,1)@ to @(d,1)@.
shearX :: (Transformable t, V t ~ R2) => Double -> t -> t
shearX = transform . shearingX

-- | @shearingY d@ is the linear transformation which is the identity on
--   x coordinates and sends @(1,0)@ to @(1,d)@.
shearingY :: Double -> T2
shearingY d = fromLinear (sh d <-> sh (-d)) (sh' d <-> sh' (-d))
  where sh  k (x,y) = (x, y+k*x)
        sh' k       = swap . sh k . swap
        swap (x,y) = (y,x)

-- | @shearY d@ performs a shear in the y-direction which sends
--   @(1,0)@ to @(1,d)@.
shearY :: (Transformable t, V t ~ R2) => Double -> t -> t
shearY = transform . shearingY

