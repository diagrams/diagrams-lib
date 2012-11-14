{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , ViewPatterns
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
       , ScaleInv(..)

       ) where

import Diagrams.Core

import Control.Newtype (over)

import Diagrams.Coordinates
import Diagrams.Transform
import Diagrams.TwoD.Size   (width, height)
import Diagrams.TwoD.Types
import Diagrams.TwoD.Vector (direction)

import Data.Semigroup

import Data.AffineSpace
import Data.VectorSpace
import Data.Basis
import Data.MemoTrie

import Control.Arrow (first, second)

-- Rotation ------------------------------------------------

-- | Create a transformation which performs a rotation by the given
--   angle.  See also 'rotate'.
rotation :: ( AdditiveGroup a
            , Floating a
            , HasBasis a
            , HasTrie (Basis a)
            , a ~ Scalar a
            , Angle m a
            ) => m a -> T2 a
rotation ang = fromLinear r (linv r)
  where
    r            = rot theta <-> rot (-theta)
    Rad theta    = convertAngle ang
    rot th (coords -> x :& y) = (cos th * x - sin th * y) & (sin th * x + cos th * y)

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
rotate :: ( Floating a
          , HasBasis a
          , HasTrie (Basis a)
          , a ~ Scalar a
          , Transformable t
          , V t ~ D2 a
          , Angle m a
          ) => m a -> t -> t
rotate = transform . rotation

-- | A synonym for 'rotate', specialized to only work with
--   @CircleFrac@ arguments; it can be more convenient to write
--   @rotateBy (1\/4)@ than @'rotate' (1\/4 :: 'CircleFrac')@.
rotateBy :: ( Floating a
            , HasBasis a
            , HasTrie (Basis a)
            , a ~ Scalar a
            , Transformable t
            , V t ~ D2 a
            ) => CircleFrac a -> t -> t
rotateBy = transform . rotation

-- | @rotationAbout p@ is a rotation about the point @p@ (instead of
--   around the local origin).
rotationAbout :: ( Angle m a
                 , Floating a
                 , HasBasis a
                 , HasTrie (Basis a)
                 , a ~ Scalar a
                 ) => P2 a -> m a -> T2 a
rotationAbout p angle = conjugate (translation (origin .-. p)) (rotation angle)

-- | @rotateAbout p@ is like 'rotate', except it rotates around the
--   point @p@ instead of around the local origin.
rotateAbout :: ( Floating a
               , HasBasis a
               , HasTrie (Basis a)
               , a ~ Scalar a
               , Transformable t
               , V t ~ D2 a
               , Angle m a
               ) => P2 a -> m a -> t -> t
rotateAbout p angle = rotate angle `under` translation (origin .-. p)

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the x (horizontal) direction.
scalingX :: ( AdditiveGroup a
            , Fractional a
            , HasBasis a
            , HasTrie (Basis a)
            , a ~ Scalar a
            ) => a -> T2 a
scalingX c = fromLinear s s
  where s = (over d2 . first) (*c) <-> (over d2 . first) (/c)

-- | Scale a diagram by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use 'scale'.
scaleX :: ( Fractional a
          , HasBasis a
          , HasTrie (Basis a)
          , a ~ Scalar a
          , Transformable t
          , V t ~ D2 a
          ) => a -> t -> t
scaleX = transform . scalingX

-- | Construct a transformation which scales by the given factor in
--   the y (vertical) direction.
scalingY :: ( AdditiveGroup a
            , Fractional a
            , HasBasis a
            , HasTrie (Basis a)
            , a ~ Scalar a
            ) => a -> T2 a
scalingY c = fromLinear s s
  where s = (over d2 . second) (*c) <-> (over d2 . second) (/c)

-- | Scale a diagram by the given factor in the y (vertical)
--   direction.  To scale uniformly, use 'scale'.
scaleY :: ( Fractional a
          , HasBasis a
          , HasTrie (Basis a)
          , a ~ Scalar a
          , Transformable t, V t ~ D2 a
          ) => a -> t -> t
scaleY = transform . scalingY

-- | @scaleToX w@ scales a diagram in the x (horizontal) direction by
--   whatever factor required to make its width @w@.  @scaleToX@
--   should not be applied to diagrams with a width of 0, such as
--   'vrule'.
scaleToX :: ( HasBasis a
            , HasTrie (Basis a)
            , a ~ Scalar a
            , Enveloped t
            , Transformable t
            , V t ~ D2 a
            ) => a -> t -> t
scaleToX w d = scaleX (w / width d) d

-- | @scaleToY h@ scales a diagram in the y (vertical) direction by
--   whatever factor required to make its height @h@.  @scaleToY@
--   should not be applied to diagrams with a height of 0, such as
--   'hrule'.
scaleToY :: (HasBasis a, HasTrie (Basis a), a ~ Scalar a, Enveloped t, Transformable t, V t ~ D2 a) => a -> t -> t
scaleToY h d = scaleY (h / height d) d

-- | @scaleUToX w@ scales a diagram /uniformly/ by whatever factor
--   required to make its width @w@.  @scaleUToX@ should not be
--   applied to diagrams with a width of 0, such as 'vrule'.
scaleUToX :: (Enveloped t, Transformable t, V t ~ D2 a) => a -> t -> t
scaleUToX w d = scale (w / width d) d

-- | @scaleUToY h@ scales a diagram /uniformly/ by whatever factor
--   required to make its height @h@.  @scaleUToY@ should not be applied
--   to diagrams with a height of 0, such as 'hrule'.
scaleUToY :: (Enveloped t, Transformable t, V t ~ D2 a) => a -> t -> t
scaleUToY h d = scale (h / height d) d

-- Translation ---------------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the x (horizontal) direction.
translationX :: ( Num a
                , HasBasis a
                , HasTrie (Basis a)
                , a ~ Scalar a
                ) => a -> T2 a
translationX x = translation (x & 0)

-- | Translate a diagram by the given distance in the x (horizontal)
--   direction.
translateX :: ( Transformable t
              , Num a
              , HasBasis a
              , HasTrie (Basis a)
              , a ~ Scalar a
              , V t ~ D2 a
              ) => a -> t -> t
translateX = transform . translationX

-- | Construct a transformation which translates by the given distance
--   in the y (vertical) direction.
translationY :: ( Num a
                , HasBasis a
                , HasTrie (Basis a)
                , a ~ Scalar a
                ) => a -> T2 a
translationY y = translation (0 & y)

-- | Translate a diagram by the given distance in the y (vertical)
--   direction.
translateY :: ( Transformable t
              , Num a
              , HasBasis a
              , HasTrie (Basis a)
              , a ~ Scalar a
              , V t ~ D2 a
              ) => a -> t -> t
translateY = transform . translationY

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips a diagram from left to
--   right, i.e. sends the point (x,y) to (-x,y).
reflectionX :: ( Fractional a
               , a ~ Scalar a
               , HasBasis a
               , HasTrie (Basis a)
               ) => T2 a
reflectionX = scalingX (-1)

-- | Flip a diagram from left to right, i.e. send the point (x,y) to
--   (-x,y).
reflectX :: ( Fractional a
            , HasBasis a
            , HasTrie (Basis a)
            , a ~ Scalar a
            , Transformable t
            , V t ~ D2 a
            ) => t -> t
reflectX = transform reflectionX

-- | Construct a transformation which flips a diagram from top to
--   bottom, i.e. sends the point (x,y) to (x,-y).
reflectionY :: ( Fractional a
               , HasBasis a
               , HasTrie (Basis a)
               , a ~ Scalar a
               ) => T2 a
reflectionY = scalingY (-1)

-- | Flip a diagram from top to bottom, i.e. send the point (x,y) to
--   (x,-y).
reflectY :: ( Fractional a
            , HasBasis a
            , HasTrie (Basis a)
            , a ~ Scalar a
            , Transformable t
            , V t ~ D2 a
            ) => t -> t
reflectY = transform reflectionY

-- | @reflectionAbout p v@ is a reflection in the line determined by
--   the point @p@ and vector @v@.
reflectionAbout :: ( a ~ Scalar a
                   , RealFloat a
                   , HasBasis a
                   , HasTrie (Basis a)
                   , Angle m a
                   ) => P2 a -> D2 a -> T2 a
reflectionAbout p v =
  conjugate (rotation (-direction v :: Rad a) <> translation (origin .-. p))
            reflectionY

-- | @reflectAbout p v@ reflects a diagram in the line determined by
--   the point @p@ and the vector @v@.
reflectAbout :: (Transformable t
                , a ~ Scalar a
                , RealFloat a
                , HasTrie (Basis a)
                , HasBasis a
                , V t ~ D2 a) => P2 a -> D2 a -> t -> t
reflectAbout p v = transform (reflectionAbout p v)

-- Shears --------------------------------------------------

-- | @shearingX d@ is the linear transformation which is the identity on
--   y coordinates and sends @(0,1)@ to @(d,1)@.
shearingX :: ( Num a
             , HasBasis a
             , HasTrie (Basis a)
             , a ~ Scalar a
             ) => a -> T2 a
shearingX d = fromLinear (over d2 (sh d)  <-> over d2 (sh (-d)))
                         (over d2 (sh' d) <-> over d2 (sh' (-d)))
  where sh  k (x, y) = (x+k*y, y)
        sh' k        = swap . sh k . swap
        swap (x,y) = (y,x)

-- | @shearX d@ performs a shear in the x-direction which sends
--   @(0,1)@ to @(d,1)@.
shearX :: ( Num a
          , HasBasis a
          , HasTrie (Basis a)
          , a ~ Scalar a
          , Transformable t
          , V t ~ D2 a
          ) => a -> t -> t
shearX = transform . shearingX

-- | @shearingY d@ is the linear transformation which is the identity on
--   x coordinates and sends @(1,0)@ to @(1,d)@.
shearingY :: (Num a, HasBasis a, HasTrie (Basis a), a ~ Scalar a) => a -> T2 a
shearingY d = fromLinear (over d2 (sh d)  <-> over d2 (sh (-d)))
                         (over d2 (sh' d) <-> over d2 (sh' (-d)))
  where sh  k (x,y) = (x, y+k*x)
        sh' k       = swap . sh k . swap
        swap (x,y) = (y,x)

-- | @shearY d@ performs a shear in the y-direction which sends
--   @(1,0)@ to @(1,d)@.
shearY :: ( Num a
          , HasBasis a
          , HasTrie (Basis a)
          , a ~ Scalar a
          , Transformable t
          , V t ~ D2 a
          ) => a -> t -> t
shearY = transform . shearingY

-- TODO
---------
---------
--Scale invariant

--1 find unit vector
--2 apply transformation to unit vector
--3 find angle difference b/w transformed unit vector and original vector
--4 rotate arrowhead
--5 add rotated arrowhead

data ScaleInv t = ScaleInv t ( R2 )
  deriving (Show )

type instance V (ScaleInv t) = R2

instance (V t ~ R2, HasOrigin t) => HasOrigin (ScaleInv t) where
  moveOriginTo p (ScaleInv s v) = ScaleInv ( moveOriginTo p s ) v 

instance (V t ~ R2, Transformable t) => Transformable (ScaleInv t) where
  transform tr (ScaleInv t v) = ScaleInv obj rotUnitVec where
        transUnitVec :: R2
        transUnitVec = transform tr v
        angle :: Rad Double
        angle = direction transUnitVec  - direction v
        rTrans :: ( Transformable t,  (V t ~ R2) ) => t -> t
        rTrans = rotate angle
        obj = rTrans t
        rotUnitVec :: R2
        rotUnitVec = rTrans v


--------

