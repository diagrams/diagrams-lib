{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
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
       , ScaleInv(..), scaleInv, scaleInvPrim

         -- * component-wise
       , onBasis
       ) where

import           Diagrams.Core
import qualified Diagrams.Core.Transform as T

import           Control.Newtype         (over)

import           Diagrams.Coordinates
import           Diagrams.Transform
import           Diagrams.TwoD.Size      (height, width)
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector    (direction)

import           Data.Semigroup

import           Data.AffineSpace

import           Control.Arrow           (first, second)

-- Rotation ------------------------------------------------

-- | Create a transformation which performs a rotation about the local
--   origin by the given angle.  See also 'rotate'.
rotation :: Angle a => a -> T2
rotation ang = fromLinear r (linv r)
  where
    r            = rot theta <-> rot (-theta)
    Rad theta    = convertAngle ang
    rot th (coords -> x :& y) = (cos th * x - sin th * y) & (sin th * x + cos th * y)

-- | Rotate about the local origin by the given angle. Positive angles
--   correspond to counterclockwise rotation, negative to
--   clockwise. The angle can be expressed using any type which is an
--   instance of 'Angle'.  For example, @rotate (1\/4 ::
--   'Turn')@, @rotate (tau\/4 :: 'Rad')@, and @rotate (90 ::
--   'Deg')@ all represent the same transformation, namely, a
--   counterclockwise rotation by a right angle.  To rotate about some
--   point other than the local origin, see 'rotateAbout'.
--
--   Note that writing @rotate (1\/4)@, with no type annotation, will
--   yield an error since GHC cannot figure out which sort of angle
--   you want to use.  In this common situation you can use
--   'rotateBy', which is specialized to take a 'Turn' argument.
rotate :: (Transformable t, V t ~ R2, Angle a) => a -> t -> t
rotate = transform . rotation

-- | A synonym for 'rotate', specialized to only work with
--   @Turn@ arguments; it can be more convenient to write
--   @rotateBy (1\/4)@ than @'rotate' (1\/4 :: 'Turn')@.
rotateBy :: (Transformable t, V t ~ R2) => Turn -> t -> t
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
  where s = (over r2 . first) (*c) <-> (over r2 . first) (/c)

-- | Scale a diagram by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use 'scale'.
scaleX :: (Transformable t, V t ~ R2) => Double -> t -> t
scaleX = transform . scalingX

-- | Construct a transformation which scales by the given factor in
--   the y (vertical) direction.
scalingY :: Double -> T2
scalingY c = fromLinear s s
  where s = (over r2 . second) (*c) <-> (over r2 . second) (/c)

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
translationX x = translation (x & 0)

-- | Translate a diagram by the given distance in the x (horizontal)
--   direction.
translateX :: (Transformable t, V t ~ R2) => Double -> t -> t
translateX = transform . translationX

-- | Construct a transformation which translates by the given distance
--   in the y (vertical) direction.
translationY :: Double -> T2
translationY y = translation (0 & y)

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
shearingX d = fromLinear (over r2 (sh d)  <-> over r2 (sh (-d)))
                         (over r2 (sh' d) <-> over r2 (sh' (-d)))
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
shearingY d = fromLinear (over r2 (sh d)  <-> over r2 (sh (-d)))
                         (over r2 (sh' d) <-> over r2 (sh' (-d)))
  where sh  k (x,y) = (x, y+k*x)
        sh' k       = swap . sh k . swap
        swap (x,y) = (y,x)

-- | @shearY d@ performs a shear in the y-direction which sends
--   @(1,0)@ to @(1,d)@.
shearY :: (Transformable t, V t ~ R2) => Double -> t -> t
shearY = transform . shearingY


-- Scale invariance ----------------------------------------

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

data ScaleInv t =
  ScaleInv
  { unScaleInv  :: t
  , scaleInvDir :: R2
  , scaleInvLoc :: P2
  }
  deriving (Show)

-- | Create a scale-invariant object pointing in the given direction.
scaleInv :: t -> R2 -> ScaleInv t
scaleInv t d = ScaleInv t d origin

type instance V (ScaleInv t) = R2

instance (V t ~ R2, HasOrigin t) => HasOrigin (ScaleInv t) where
  moveOriginTo p (ScaleInv t v l) = ScaleInv (moveOriginTo p t) v (moveOriginTo p l)

instance (V t ~ R2, Transformable t) => Transformable (ScaleInv t) where
  transform tr (ScaleInv t v l) = ScaleInv (trans . rot $ t) (rot v) l'
    where
      angle :: Rad
      angle = direction (transform tr v) - direction v
      rot :: (Transformable t, V t ~ R2) => t -> t
      rot = rotateAbout l angle
      l'  = transform tr l
      trans = translate (l' .-. l)

-- This is how we handle freezing properly with ScaleInv wrappers.
-- Normal transformations are applied ignoring scaling; "frozen"
-- transformations (i.e. transformations applied after a freeze) are
-- applied directly to the underlying object, scales and all.  We must
-- take care to transform the reference point and direction vector
-- appropriately.
instance (V t ~ R2, Transformable t) => IsPrim (ScaleInv t) where
  transformWithFreeze t1 t2 s = ScaleInv t'' d'' origin''
    where
      -- first, apply t2 normally, i.e. ignoring scaling
      s'@(ScaleInv t' _ _)      = transform t2 s

      -- now apply t1 to get the new direction and origin
      (ScaleInv _ d'' origin'') = transform t1 s'

      -- but apply t1 directly to the underlying thing, scales and all.
      t''                       = transform t1 t'

instance (Renderable t b, V t ~ R2) => Renderable (ScaleInv t) b where
  render b = render b . unScaleInv

-- | Create a diagram from a single scale-invariant primitive, which
--   will have an /empty/ envelope, trace, and query.  The reason is
--   that the envelope, trace, and query cannot be cached---applying a
--   transformation would cause the cached envelope, etc. to get "out
--   of sync" with the scale-invariant object.  The intention, at any
--   rate, is that scale-invariant things will be used only as
--   "decorations" (/e.g./ arrowheads) which should not affect the
--   envelope, trace, and query.
scaleInvPrim :: (Transformable t, Renderable t b, V t ~ R2, Monoid m)
             => t -> R2 -> QDiagram b R2 m
scaleInvPrim t d = mkQD (Prim $ scaleInv t d) mempty mempty mempty mempty

-- | Get the matrix equivalent of the linear transform,
--   (as a pair of columns) and the translation vector.  This
--   is mostly useful for implementing backends.
onBasis :: Transformation R2 -> ((R2, R2), R2)
onBasis t = ((x, y), v)
  where ((x:y:[]), v) = T.onBasis t
