{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Transform
-- Copyright   :  (c) 2011-2015 diagrams-lib team (see LICENSE)
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
       , rotation, rotate, rotateBy, rotated

       , rotationAround, rotateAround
       , rotationTo, rotateTo

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
       , reflectionXY, reflectXY
       , reflectionAbout, reflectAbout
       , reflectionAbout', reflectAbout'

         -- * Shears
       , shearingX, shearX
       , shearingY, shearY

       ) where

import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.Core.Transform
import           Diagrams.Direction
import           Diagrams.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector

import           Control.Lens            hiding (at, transform)
import           Data.Semigroup

import           Linear.Affine
import           Linear.Vector
import           Linear.V2

-- Rotation ------------------------------------------------

-- | Create a transformation which performs a rotation about the local
--   origin by the given angle.  See also 'rotate'.


rotation :: Floating n => Angle n -> T2 n
rotation theta = fromLinear r (linv r)
    where
    c = cosA theta
    s = sinA theta
    r               = rot c s <-> rot c (-s)
    rot co si (V2 x y) = V2 (co * x - si * y)
                            (si * x + co * y)


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

rotate :: (InSpace V2 n t, Transformable t, Floating n) => Angle n -> t -> t
rotate = transform . rotation

-- | A synonym for 'rotate', interpreting its argument in units of
-- turns; it can be more convenient to write @rotateBy (1\/4)@ than
-- @'rotate' (1\/4 \@\@ 'turn')@.
rotateBy :: (InSpace V2 n t, Transformable t, Floating n) => n -> t -> t
rotateBy = transform . rotation . review turn

-- | Use an 'Angle' to make an 'Iso' between an object
--   rotated and unrotated. This us useful for performing actions
--   'under' a rotation:
--
-- @
-- under (rotated t) f = rotate (negated t) . f . rotate t
-- rotated t ## a      = rotate t a
-- a ^. rotated t      = rotate (-t) a
-- over (rotated t) f  = rotate t . f . rotate (negated t)
-- @
rotated :: (InSpace V2 n a, Floating n, SameSpace a b, Transformable a, Transformable b)
        => Angle n -> Iso a b a b
rotated = transformed . rotation

-- | @rotationAbout p@ is a rotation about the point @p@ (instead of
--   around the local origin).
rotationAround :: Floating n => P2 n -> Angle n -> T2 n
rotationAround p theta =
  conjugate (translation (origin .-. p)) (rotation theta)

-- | @rotateAbout p@ is like 'rotate', except it rotates around the
--   point @p@ instead of around the local origin.
rotateAround :: (InSpace V2 n t, Transformable t, Floating n)
             => P2 n -> Angle n -> t -> t
rotateAround p theta = transform (rotationAround p theta)

-- | The rotation that aligns the x-axis with the given direction.
rotationTo :: OrderedField n => Direction V2 n -> T2 n
rotationTo (view _Dir -> V2 x y) = rotation (atan2A' y x)

-- | Rotate around the local origin such that the x axis aligns with the
--   given direction.
rotateTo :: (InSpace V2 n t, OrderedField n, Transformable t) => Direction V2 n -> t -> t
rotateTo = transform . rotationTo

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the x (horizontal) direction.
scalingX :: (Additive v, R1 v, Fractional n) => n -> Transformation v n
scalingX c = fromSymmetric $ (_x *~ c) <-> (_x //~ c)

-- | Scale a diagram by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use 'scale'.
scaleX :: (InSpace v n t, R2 v, Fractional n, Transformable t) => n -> t -> t
scaleX = transform . scalingX

-- | Construct a transformation which scales by the given factor in
--   the y (vertical) direction.
scalingY :: (Additive v, R2 v, Fractional n) => n -> Transformation v n
scalingY c = fromSymmetric $ (_y *~ c) <-> (_y //~ c)

-- | Scale a diagram by the given factor in the y (vertical)
--   direction.  To scale uniformly, use 'scale'.
scaleY :: (InSpace v n t, R2 v, Fractional n, Transformable t)
  => n -> t -> t
scaleY = transform . scalingY

-- | @scaleToX w@ scales a diagram in the x (horizontal) direction by
--   whatever factor required to make its width @w@.  @scaleToX@
--   should not be applied to diagrams with a width of 0, such as
--   'vrule'.
scaleToX :: (InSpace v n t, R2 v, Enveloped t, Transformable t) => n -> t -> t
scaleToX w d = scaleX (w / diameter unitX d) d

-- | @scaleToY h@ scales a diagram in the y (vertical) direction by
--   whatever factor required to make its height @h@.  @scaleToY@
--   should not be applied to diagrams with a height of 0, such as
--   'hrule'.
scaleToY :: (InSpace v n t, R2 v, Enveloped t, Transformable t) => n -> t -> t
scaleToY h d = scaleY (h / diameter unitY d) d

-- | @scaleUToX w@ scales a diagram /uniformly/ by whatever factor
--   required to make its width @w@.  @scaleUToX@ should not be
--   applied to diagrams with a width of 0, such as 'vrule'.
scaleUToX :: (InSpace v n t, R1 v, Enveloped t, Transformable t) => n -> t -> t
scaleUToX w d = scale (w / diameter unitX d) d

-- | @scaleUToY h@ scales a diagram /uniformly/ by whatever factor
--   required to make its height @h@.  @scaleUToY@ should not be applied
--   to diagrams with a height of 0, such as 'hrule'.
scaleUToY :: (InSpace v n t, R2 v, Enveloped t, Transformable t) => n -> t -> t
scaleUToY h d = scale (h / diameter unitY d) d

-- Translation ---------------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the x (horizontal) direction.
translationX :: (Additive v, R1 v, Num n) => n -> Transformation v n
translationX x = translation (zero & _x .~ x)

-- | Translate a diagram by the given distance in the x (horizontal)
--   direction.
translateX :: (InSpace v n t, R1 v, Transformable t) => n -> t -> t
translateX = transform . translationX

-- | Construct a transformation which translates by the given distance
--   in the y (vertical) direction.
translationY :: (Additive v, R2 v, Num n) => n -> Transformation v n
translationY y = translation (zero & _y .~ y)

-- | Translate a diagram by the given distance in the y (vertical)
--   direction.
translateY :: (InSpace v n t, R2 v, Transformable t)
  => n -> t -> t
translateY = transform . translationY



-- Reflection ----------------------------------------------

-- | Construct a transformation which flips a diagram from left to
--   right, i.e. sends the point (x,y) to (-x,y).
reflectionX :: (Additive v, R1 v, Num n) => Transformation v n
reflectionX = fromSymmetric $ (_x *~ (-1)) <-> (_x *~ (-1))

-- | Flip a diagram from left to right, i.e. send the point (x,y) to
--   (-x,y).
reflectX :: (InSpace v n t, R1 v, Transformable t) => t -> t
reflectX = transform reflectionX

-- | Construct a transformation which flips a diagram from top to
--   bottom, i.e. sends the point (x,y) to (x,-y).
reflectionY :: (Additive v, R2 v, Num n) => Transformation v n
reflectionY = fromSymmetric $ (_y *~ (-1)) <-> (_y *~ (-1))

-- | Flip a diagram from top to bottom, i.e. send the point (x,y) to
--   (x,-y).
reflectY :: (InSpace v n t, R2 v, Transformable t) => t -> t
reflectY = transform reflectionY

-- | Construct a transformation which flips the diagram about x=y, i.e.
--   sends the point (x,y) to (y,x).
reflectionXY :: (Additive v, R2 v, Num n) => Transformation v n
reflectionXY = fromSymmetric $ (_xy %~ view _yx) <-> (_xy %~ view _yx)

-- | Flips the diagram about x=y, i.e. send the point (x,y) to (y,x).
reflectXY :: (InSpace v n t, R2 v, Transformable t) => t -> t
reflectXY = transform reflectionXY

-- | @reflectionAbout p d@ is a reflection in the line determined by
--   the point @p@ and direction @d@.
reflectionAbout :: OrderedField n => P2 n -> Direction V2 n -> T2 n
reflectionAbout p d =
  conjugate (rotationTo d <> translation (origin .-. p))
            reflectionY


reflectionAbout' :: OrderedField n => P2 n -> V2 n -> T2 n
reflectionAbout' p d  = fromLinear r (linv r)
              where
                r                           = refl p d <-> refl p d
                refl (P (V2 a b )) (V2 c d) =
                 (combine (translate (V2 a b))
                 (combine (rotate (atan2A' d c) )
                 (combine (reflectY)
                 (combine (rotate((-1) *^ (atan2A' d c) ) )
                 (translate (V2 (-a) (-b)) )))) )
combine :: (a -> a) -> (a -> a) -> a -> a
combine f g c = f (g c)
--l :: (a -> a) -> (a -> a) -> (a -> a)
--l f g = f g
--(combine f g) c != ( f g ) c
--f (g (c)) = ???
                 --(translate (V2 a b)   ((rotate (atan2A' d c) )   ((reflectY)  ((rotate(negated (atan2A' d c) ) )  (translate (V2 (-a) (-b)) )))))
                 --refl (P (V2 a b )) (V2 c d) = translate (V2 a b)  (over (rotated (atan2A' d c)) reflectY) (translate (V2 (-a) (-b)) )


reflectAbout' :: (InSpace V2 n t, OrderedField n, Transformable t)
                             => P2 n -> V2 n -> t -> t
reflectAbout' p v = transform (reflectionAbout' p v)

--reflectAbout' (P (V2 a b )) (V2 x y) = translate' a b (over (rotated (atan2A' y x)) reflectY) translate' (-a) (-b)


-- | @reflectAbout p d@ reflects a diagram in the line determined by
--   the point @p@ and direction @d@.
reflectAbout :: (InSpace V2 n t, OrderedField n, Transformable t)
             => P2 n -> Direction V2 n -> t -> t
reflectAbout p v = transform (reflectionAbout p v)

--reflectAbout' :: (InSpace V2 n t, OrderedField n, Transformable t)
  --           => P2 n -> V2 n -> t -> t
--reflectAbout' p v = transform (reflectionAbout' p v)
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
shearX :: (InSpace V2 n t, Transformable t) => n -> t -> t
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
shearY :: (InSpace V2 n t, Transformable t) => n -> t -> t
shearY = transform . shearingY
