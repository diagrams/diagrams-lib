{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
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
         T2
         -- * Rotation
       , rotation, rotate, rotateBy

       , rotationAround, rotateAround

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

         -- * Utilities
       -- , onBasis
       ) where

import           Diagrams.Core

import           Diagrams.Angle
import           Diagrams.Transform
import           Diagrams.TwoD.Size  (height, width)
import           Diagrams.TwoD.Types

import           Control.Lens        (review, (&), (*~), (.~), (//~), (^.))
import           Data.Semigroup

import           Linear.Affine
import           Linear.Vector

type T2 = Transformation V2

-- Rotation ------------------------------------------------

-- | Create a transformation which performs a rotation about the local
--   origin by the given angle.  See also 'rotate'.
rotation :: Floating n => Angle n -> T2 n
rotation ang = fromLinear r (linv r)
  where
    r                      = rot theta <-> rot (-theta)
    theta                  = ang^.rad
    rot th (unr2 -> (x,y)) = mkR2 (cos th * x - sin th * y) (sin th * x + cos th * y)

-- | Rotate about the local origin by the given angle. Positive angles
--   correspond to counterclockwise rotation, negative to
--   clockwise. The angle can be expressed using any of the 'Iso's on
--   'Angle'.  For example, @rotate (1\/4 \@\@ 'turn')@, @rotate
--   (tau\/4 \@\@ rad)@, and @rotate (90 \@\@ deg)@ all
--   represent the same transformation, namely, a counterclockwise
--   rotation by a right angle.  To rotate about some point other than
--   the local origin, see 'rotateAbout'.
--
--   Note that writing @rotate (1\/4)@, with no 'Angle' constructor,
--   will yield an error since GHC cannot figure out which sort of
--   angle you want to use.  In this common situation you can use
--   'rotateBy', which interprets its argument as a number of turns.
rotate :: (V t ~ V2, N t ~ n, Transformable t, Floating n) => Angle n -> t -> t
rotate = transform . rotation

-- | A synonym for 'rotate', interpreting its argument in units of
-- turns; it can be more convenient to write @rotateBy (1\/4)@ than
-- @'rotate' (1\/4 \@\@ 'turn')@.
rotateBy :: (V t ~ V2, N t ~ n, Transformable t, Floating n) => n -> t -> t
rotateBy = transform . rotation . review turn

-- | @rotationAbout p@ is a rotation about the point @p@ (instead of
--   around the local origin).
rotationAround :: Floating n => P2 n -> Angle n -> T2 n
rotationAround p angle = conjugate (translation (origin .-. p)) (rotation angle)

-- | @rotateAbout p@ is like 'rotate', except it rotates around the
--   point @p@ instead of around the local origin.
rotateAround :: (V t ~ V2, N t ~ n, Transformable t, Floating n) => P2 n -> Angle n -> t -> t
rotateAround p angle = rotate angle `under` translation (origin .-. p)

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the x (horizontal) direction.
scalingX :: (R1 v, Additive v, Floating n) => n -> Transformation v n
scalingX c = fromLinear s s
  where s = (_x *~ c) <-> (_x //~ c)

-- | Scale a diagram by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use 'scale'.
scaleX :: (V t ~ v, N t ~ n, Transformable t, R1 v, Additive v, Floating n)
  => n -> t -> t
scaleX = transform . scalingX

-- | Construct a transformation which scales by the given factor in
--   the y (vertical) direction.
scalingY :: (R2 v, Additive v, Floating n) => n -> Transformation v n
scalingY c = fromLinear s s
  where s = (_y *~ c) <-> (_y //~ c)

-- | Scale a diagram by the given factor in the y (vertical)
--   direction.  To scale uniformly, use 'scale'.
scaleY :: (V t ~ v, N t ~ n, Transformable t, R2 v, Additive v, Floating n)
  => n -> t -> t
scaleY = transform . scalingY

-- | @scaleToX w@ scales a diagram in the x (horizontal) direction by
--   whatever factor required to make its width @w@.  @scaleToX@
--   should not be applied to diagrams with a width of 0, such as
--   'vrule'.
scaleToX :: (V t ~ V2, N t ~ n, Enveloped t, Transformable t) => n -> t -> t
scaleToX w d = scaleX (w / width d) d

-- | @scaleToY h@ scales a diagram in the y (vertical) direction by
--   whatever factor required to make its height @h@.  @scaleToY@
--   should not be applied to diagrams with a height of 0, such as
--   'hrule'.
scaleToY :: (V t ~ V2, N t ~ n, Enveloped t, Transformable t) => n -> t -> t
scaleToY h d = scaleY (h / height d) d

-- | @scaleUToX w@ scales a diagram /uniformly/ by whatever factor
--   required to make its width @w@.  @scaleUToX@ should not be
--   applied to diagrams with a width of 0, such as 'vrule'.
scaleUToX :: (V t ~ V2, N t ~ n, Enveloped t, Transformable t) => n -> t -> t
scaleUToX w d = scale (w / width d) d

-- | @scaleUToY h@ scales a diagram /uniformly/ by whatever factor
--   required to make its height @h@.  @scaleUToY@ should not be applied
--   to diagrams with a height of 0, such as 'hrule'.
scaleUToY :: (V t ~ V2, N t ~ n, Enveloped t, Transformable t) => n -> t -> t
scaleUToY h d = scale (h / height d) d

-- Translation ---------------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the x (horizontal) direction.
translationX :: (R1 v, Additive v, Floating n) => n -> Transformation v n
translationX x = translation (zero & _x .~ x)

-- | Translate a diagram by the given distance in the x (horizontal)
--   direction.
translateX :: (V t ~ v, N t ~ n, Transformable t, R1 v, Additive v, Floating n)
  => n -> t -> t
translateX = transform . translationX

-- | Construct a transformation which translates by the given distance
--   in the y (vertical) direction.
translationY :: (R2 v, Additive v, Floating n) => n -> Transformation v n
translationY y = translation (zero & _y .~ y)

-- | Translate a diagram by the given distance in the y (vertical)
--   direction.
translateY :: (V t ~ v, N t ~ n, Transformable t, R2 v, Additive v, Floating n)
  => n -> t -> t
translateY = transform . translationY

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips a diagram from left to
--   right, i.e. sends the point (x,y) to (-x,y).
reflectionX :: (R1 v, Additive v, Floating n) => Transformation v n
reflectionX = scalingX (-1)

-- | Flip a diagram from left to right, i.e. send the point (x,y) to
--   (-x,y).
reflectX :: (V t ~ v, N t ~ n, Transformable t, R1 v, Additive v, Floating n) => t -> t
reflectX = transform reflectionX

-- | Construct a transformation which flips a diagram from top to
--   bottom, i.e. sends the point (x,y) to (x,-y).
reflectionY :: (R2 v, Additive v, Floating n) => Transformation v n
reflectionY = scalingY (-1)

-- | Flip a diagram from top to bottom, i.e. send the point (x,y) to
--   (x,-y).
reflectY :: (V t ~ v, N t ~ n, Transformable t, R2 v, Additive v, Floating n)
  => t -> t
reflectY = transform reflectionY

-- | @reflectionAbout p v@ is a reflection in the line determined by
--   the point @p@ and vector @v@.
reflectionAbout :: RealFloat n => P2 n -> V2 n -> T2 n
reflectionAbout p v =
  conjugate (rotation (negated $ v ^. _theta) <> translation (origin .-. p))
            reflectionY

-- | @reflectAbout p v@ reflects a diagram in the line determined by
--   the point @p@ and the vector @v@.
reflectAbout :: (V t ~ V2, N t ~ n, Transformable t, RealFloat n) => P2 n -> V2 n -> t -> t
reflectAbout p v = transform (reflectionAbout p v)

-- Shears --------------------------------------------------

-- auxiliary functions for shearingX/shearingY
sh :: (n -> n -> n -> n) -> (n -> n -> n -> n) -> n -> V2 n -> V2 n
sh f g k (V2 x y) = V2 (f k x y) (g k x y)

sh' :: (n -> n -> n -> n) -> (n -> n -> n -> n) -> n -> V2 n -> V2 n
sh' f g k = swap . sh f g k . swap

swap :: V2 n -> V2 n
swap (V2 x y) = V2 y x
{-# INLINE swap #-}

-- | @shearingX d@ is the linear transformation which is the identity on
--   y coordinates and sends @(0,1)@ to @(d,1)@.
shearingX :: Num n => n -> T2 n
shearingX d = fromLinear (sh f g d  <-> sh f g (-d))
                         (sh' f g d <-> sh' f g (-d))
  where
    f k x y = x + k*y
    g _ _ y = y

-- | @shearX d@ performs a shear in the x-direction which sends
--   @(0,1)@ to @(d,1)@.
shearX :: (V t ~ V2, N t ~ n, Transformable t, Num n) => n -> t -> t
shearX = transform . shearingX

-- | @shearingY d@ is the linear transformation which is the identity on
--   x coordinates and sends @(1,0)@ to @(1,d)@.
shearingY :: Num n => n -> T2 n
shearingY d = fromLinear (sh f g d  <-> sh f g (-d))
                         (sh' f g d <-> sh' f g (-d))
        where
          f _ x _ = x
          g k x y = y + k*x

-- | @shearY d@ performs a shear in the y-direction which sends
--   @(1,0)@ to @(1,d)@.
shearY :: (V t ~ V2, N t ~ n, Transformable t, Num n) => n -> t -> t
shearY = transform . shearingY

-- | Get the matrix equivalent of the linear transform,
--   (as a pair of columns) and the translation vector.  This
--   is mostly useful for implementing backends.
-- onBasis :: T v -> ((v, v), v)
-- onBasis t = ((x, y), v)
--   where (x:y:[], v) = T.onBasis t
