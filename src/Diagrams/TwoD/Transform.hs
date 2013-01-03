{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeFamilies
           , ViewPatterns
  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
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

         -- * Scale invariance
       , ScaleInv(..), scaleInv

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
rotation :: ( Floating a
            , HasBasis a
            , HasTrie (Basis a)
            , Angle m a
            ) => m a -> T2 a
rotation ang = fromLinear r (linv r)
  where
    r            = rot theta <-> rot (-theta)
    Rad theta    = convertAngle ang
    rot th (coords -> x :& y) = (cos th * x - sin th * y) & (sin th * x + cos th * y)

-- | Rotate about the local origin by the given angle. Positive angles
--   correspond to counterclockwise rotation, negative to
--   clockwise. The angle can be expressed using any type which is an
--   instance of 'Angle'.  For example, @rotate (1\/4 ::
--   'CircleFrac')@, @rotate (tau\/4 :: 'Rad')@, and @rotate (90 ::
--   'Deg')@ all represent the same transformation, namely, a
--   counterclockwise rotation by a right angle.  To rotate about some
--   point other than the local origin, see 'rotateAbout'.
--
--   Note that writing @rotate (1\/4)@, with no type annotation, will
--   yield an error since GHC cannot figure out which sort of angle
--   you want to use.  In this common situation you can use
--   'rotateBy', which is specialized to take a 'CircleFrac' argument.
rotate :: ( Floating a
          , HasBasis a
          , HasTrie (Basis a)
          , Transformable t
          , V t ~ V2 a
          , Angle m a
          ) => m a -> t -> t
rotate = transform . rotation

-- | A synonym for 'rotate', specialized to only work with
--   @CircleFrac@ arguments; it can be more convenient to write
--   @rotateBy (1\/4)@ than @'rotate' (1\/4 :: 'CircleFrac')@.
rotateBy :: ( Floating a
            , HasBasis a
            , HasTrie (Basis a)
            , Transformable t
            , V t ~ V2 a
            ) => CircleFrac a -> t -> t
rotateBy = transform . rotation

-- | @rotationAbout p@ is a rotation about the point @p@ (instead of
--   around the local origin).
rotationAbout :: ( Angle m a
                 , Floating a
                 , HasBasis a
                 , HasTrie (Basis a)
                 ) => P2 a -> m a -> T2 a
rotationAbout p angle = conjugate (translation (origin .-. p)) (rotation angle)

-- | @rotateAbout p@ is like 'rotate', except it rotates around the
--   point @p@ instead of around the local origin.
rotateAbout :: ( Floating a
               , HasBasis a
               , HasTrie (Basis a)
               , Transformable t
               , V t ~ V2 a
               , Angle m a
               ) => P2 a -> m a -> t -> t
rotateAbout p angle = rotate angle `under` translation (origin .-. p)

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the x (horizontal) direction.
scalingX :: ( Fractional (Scalar (V2 a))
            , HasBasis a
            , HasTrie (Basis a)
            ) => Scalar (V2 a) -> T2 a
scalingX c = fromLinear s s
  where s = (over v2 . first) (^* c) <-> (over v2 . first) (^/ c)

-- | Scale a diagram by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use 'scale'.
scaleX :: ( Fractional (Scalar (V2 a))
          , HasBasis a
          , HasTrie (Basis a)
          , Transformable t
          , V t ~ V2 a
          ) => Scalar (V2 a) -> t -> t
scaleX = transform . scalingX

-- | Construct a transformation which scales by the given factor in
--   the y (vertical) direction.
scalingY :: ( Fractional (Scalar (V2 a))
            , HasBasis a
            , HasTrie (Basis a)
            ) => Scalar (V2 a) -> T2 a
scalingY c = fromLinear s s
  where s = (over v2 . second) (^* c) <-> (over v2 . second) (^/ c)

-- | Scale a diagram by the given factor in the y (vertical)
--   direction.  To scale uniformly, use 'scale'.
scaleY :: ( Fractional (Scalar (V2 a))
          , HasBasis a
          , HasTrie (Basis a)
          , Transformable t, V t ~ V2 a
          ) => Scalar (V2 a) -> t -> t
scaleY = transform . scalingY

-- | @scaleToX w@ scales a diagram in the x (horizontal) direction by
--   whatever factor required to make its width @w@.  @scaleToX@
--   should not be applied to diagrams with a width of 0, such as
--   'vrule'.
scaleToX :: ( Num a
            , HasBasis a
            , HasTrie (Basis a)
            , Enveloped t
            , Transformable t
            , V t ~ V2 a
            ) => Scalar (V2 a) -> t -> t
scaleToX w d = scaleX (w / width d) d

-- | @scaleToY h@ scales a diagram in the y (vertical) direction by
--   whatever factor required to make its height @h@.  @scaleToY@
--   should not be applied to diagrams with a height of 0, such as
--   'hrule'.
scaleToY :: ( Num a
            , HasBasis a
            , HasTrie (Basis a)
            , Enveloped t
            , Transformable t
            , V t ~ V2 a
            ) => Scalar (V2 a) -> t -> t
scaleToY h d = scaleY (h / height d) d

-- | @scaleUToX w@ scales a diagram /uniformly/ by whatever factor
--   required to make its width @w@.  @scaleUToX@ should not be
--   applied to diagrams with a width of 0, such as 'vrule'.
scaleUToX :: ( Num a
             , AdditiveGroup a
             , Enveloped t
             , Transformable t
             , V t ~ V2 a
             ) => Scalar (V2 a) -> t -> t
scaleUToX w d = scale (w / width d) d

-- | @scaleUToY h@ scales a diagram /uniformly/ by whatever factor
--   required to make its height @h@.  @scaleUToY@ should not be applied
--   to diagrams with a height of 0, such as 'hrule'.
scaleUToY :: ( Num a
             , AdditiveGroup a
             , Enveloped t
             , Transformable t
             , V t ~ V2 a
             ) => Scalar (V2 a) -> t -> t
scaleUToY h d = scale (h / height d) d

-- Translation ---------------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the x (horizontal) direction.
translationX :: ( Num a
                , HasBasis a
                , HasTrie (Basis a)
                ) => a -> T2 a
translationX x = translation (x & 0)

-- | Translate a diagram by the given distance in the x (horizontal)
--   direction.
translateX :: ( Transformable t
              , Num a
              , HasBasis a
              , HasTrie (Basis a)
              , V t ~ V2 a
              ) => a -> t -> t
translateX = transform . translationX

-- | Construct a transformation which translates by the given distance
--   in the y (vertical) direction.
translationY :: ( Num a
                , HasBasis a
                , HasTrie (Basis a)
                ) => a -> T2 a
translationY y = translation (0 & y)

-- | Translate a diagram by the given distance in the y (vertical)
--   direction.
translateY :: ( Transformable t
              , Num a
              , HasBasis a
              , HasTrie (Basis a)
              , V t ~ V2 a
              ) => a -> t -> t
translateY = transform . translationY

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips a diagram from left to
--   right, i.e. sends the point (x,y) to (-x,y).
reflectionX :: ( Fractional (Scalar (V2 a))
               , HasBasis a
               , HasTrie (Basis a)
               ) => T2 a
reflectionX = scalingX (-1)

-- | Flip a diagram from left to right, i.e. send the point (x,y) to
--   (-x,y).
reflectX :: ( Fractional (Scalar (V2 a))
            , HasBasis a
            , HasTrie (Basis a)
            , Transformable t
            , V t ~ V2 a
            ) => t -> t
reflectX = transform reflectionX

-- | Construct a transformation which flips a diagram from top to
--   bottom, i.e. sends the point (x,y) to (x,-y).
reflectionY :: ( Fractional (Scalar (V2 a))
               , HasBasis a
               , HasTrie (Basis a)
               ) => T2 a
reflectionY = scalingY (-1)

-- | Flip a diagram from top to bottom, i.e. send the point (x,y) to
--   (x,-y).
reflectY :: ( Fractional (Scalar (V2 a))
            , HasBasis a
            , HasTrie (Basis a)
            , Transformable t
            , V t ~ V2 a
            ) => t -> t
reflectY = transform reflectionY

-- | @reflectionAbout p v@ is a reflection in the line determined by
--   the point @p@ and vector @v@.
reflectionAbout :: ( RealFloat a
                   , Fractional (Scalar (V2 a))
                   , HasBasis a
                   , HasTrie (Basis a)
                   , Angle Rad a
                   ) => P2 a -> V2 a -> T2 a
reflectionAbout p v =
  conjugate (rotation (rad $ -direction v) <> translation (origin .-. p))
            reflectionY

-- | @reflectAbout p v@ reflects a diagram in the line determined by
--   the point @p@ and the vector @v@.
reflectAbout :: ( Transformable t
                , Fractional (Scalar (V2 a))
                , RealFloat a
                , HasTrie (Basis a)
                , HasBasis a
                , V t ~ V2 a) => P2 a -> V2 a -> t -> t
reflectAbout p v = transform (reflectionAbout p v)

-- Shears --------------------------------------------------

-- | @shearingX d@ is the linear transformation which is the identity on
--   y coordinates and sends @(0,1)@ to @(d,1)@.
shearingX :: ( Num a
             , HasBasis a
             , HasTrie (Basis a)
             ) => a -> T2 a
shearingX d = fromLinear (over v2 (sh d)  <-> over v2 (sh (-d)))
                         (over v2 (sh' d) <-> over v2 (sh' (-d)))
  where sh  k (x, y) = (x+k *y, y)
        sh' k        = swap . sh k . swap
        swap (x,y) = (y,x)

-- | @shearX d@ performs a shear in the x-direction which sends
--   @(0,1)@ to @(d,1)@.
shearX :: ( Num a
          , HasBasis a
          , HasTrie (Basis a)
          , Transformable t
          , V t ~ V2 a
          ) => a -> t -> t
shearX = transform . shearingX

-- | @shearingY d@ is the linear transformation which is the identity on
--   x coordinates and sends @(1,0)@ to @(1,d)@.
shearingY :: ( Num a
             , HasBasis a
             , HasTrie (Basis a)
             ) => a -> T2 a
shearingY d = fromLinear (over v2 (sh d)  <-> over v2 (sh (-d)))
                         (over v2 (sh' d) <-> over v2 (sh' (-d)))
  where sh  k (x,y) = (x, y+k*x)
        sh' k       = swap . sh k . swap
        swap (x,y) = (y,x)

-- | @shearY d@ performs a shear in the y-direction which sends
--   @(1,0)@ to @(1,d)@.
shearY :: ( Num a
          , HasBasis a
          , HasTrie (Basis a)
          , Transformable t
          , V t ~ V2 a
          ) => a -> t -> t
shearY = transform . shearingY

-- Scale invariance ----------------------------------------

-- XXX what about freezing?  Doesn't interact with ScaleInv the way it
-- ought.

-- | The @ScaleInv@ wrapper creates two-dimensional /scale-invariant/
--   objects.  Intuitively, a scale-invariant object is affected by
--   transformations like translations and rotations, but not by scales.
--
--   However, this is problematic when it comes to /non-uniform/
--   scales (/e.g./ @scaleX 2 . scaleY 3@) since they can introduce a
--   perceived rotational component.  The prototypical example is an
--   arrowhead on the end of a path, which should be scale-invariant.
--   However, applying a non-uniform scale to the path but not the
--   arrowhead would leave the arrowhead pointing in the wrong
--   direction.
--
--   Moreover, for objects whose local origin is not at the local
--   origin of the parent diagram, any scale can result in a
--   translational component as well.
--
--   The solution is to also store a point (indicating the location,
--   /i.e./ the local origin) and a unit vector (indicating the
--   /direction/) along with a scale-invariant object.  A
--   transformation to be applied is decomposed into rotational and
--   translational components as follows:
--
--   * The transformation is applied to the direction vector, and the
--   difference in angle between the original direction vector and its
--   image under the transformation determines the rotational
--   component.  The rotation is applied with respect to the stored
--   location, rather than the global origin.
--
--   * The vector from the location to the image of the location under
--   the transformation determines the translational component.

data ScaleInv t a =
  ScaleInv
  { unScaleInv :: t
  , scaleInvDir :: V2 a
  , scaleInvLoc :: P2 a
  }
  deriving (Show)

-- | Create a scale-invariant object pointing in the given direction.
scaleInv :: (AdditiveGroup a) => t -> V2 a -> ScaleInv t a
scaleInv t d = ScaleInv t d origin

type instance V (ScaleInv t a) = V2 a

instance (V t ~ V2 a, HasOrigin t) => HasOrigin (ScaleInv t a) where
  moveOriginTo p (ScaleInv t v l) = ScaleInv (moveOriginTo p t) v (moveOriginTo p l)

instance ( RealFloat a
         , HasBasis a
         , HasTrie (Basis a)
         , Transformable (V2 a)
         ) => Transformable (ScaleInv (V2 a) a) where
  transform tr (ScaleInv t v l) = ScaleInv (trans . rot $ t) (rot v) l'
    where
      angle = rad $ direction (transform tr v) - direction v
      rot :: ( Transformable t,  (V t ~ V2 a) ) => t -> t
      rot = rotateAbout l angle
      l'  = transform tr l
      trans = translate (l' .-. l)

