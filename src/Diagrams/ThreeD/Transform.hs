{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns         , ConstraintKinds, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Transform
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Transformations specific to three dimensions, with a few generic
-- transformations (uniform scaling, translation) also re-exported for
-- convenience.
--
-----------------------------------------------------------------------------

module Diagrams.ThreeD.Transform
       (
         -- * Rotation
         aboutX, aboutY, aboutZ
       , rotationAbout, pointAt, pointAt'

       -- * Scaling
       , scalingX, scalingY, scalingZ
       , scaleX, scaleY, scaleZ
       , scaling, scale

       -- * Translation
       , translationX, translateX
       , translationY, translateY
       , translationZ, translateZ
       , translation, translate

         -- * Reflection
       , reflectionX, reflectX
       , reflectionY, reflectY
       , reflectionZ, reflectZ
       , reflectionAbout, reflectAbout

       -- * Utilities for Backends
       , onBasis
       ) where

import           Diagrams.Core
import qualified Diagrams.Core.Transform as T

import           Diagrams.Angle
import           Diagrams.Direction
import           Diagrams.Transform
import           Diagrams.ThreeD.Types
import           Diagrams.Coordinates

import           Control.Lens                   (view, (*~), (//~))
import           Data.Semigroup

import           Data.AffineSpace
import           Data.Cross
import           Data.VectorSpace

-- | Create a transformation which rotates by the given angle about
--   a line parallel the Z axis passing through the local origin.
--   A positive angle brings positive x-values towards the positive-y axis.
--
--   The angle can be expressed using any type which is an
--   instance of 'Angle'.  For example, @aboutZ (1\/4 \@\@
--   'turn')@, @aboutZ (tau\/4 \@\@ 'rad')@, and @aboutZ (90 \@\@
--   'deg')@ all represent the same transformation, namely, a
--   counterclockwise rotation by a right angle.  For more general rotations,
--   see 'rotationAbout'.
--
--   Note that writing @aboutZ (1\/4)@, with no type annotation, will
--   yield an error since GHC cannot figure out which sort of angle
--   you want to use.
aboutZ :: (R3Ish v) => Angle (Scalar v) -> Transformation v
aboutZ ang = fromLinear r (linv r) where
  r = rot theta <-> rot (-theta)
  theta = view rad ang
  rot th (coords -> x :& y :& z) = (cos th * x - sin th * y) ^&
                                   (sin th * x + cos th * y) ^&
                                   z

-- | Like 'aboutZ', but rotates about the X axis, bringing positive y-values
-- towards the positive z-axis.
aboutX :: (R3Ish v) => Angle (Scalar v) -> Transformation v
aboutX ang = fromLinear r (linv r) where
  r = rot theta <-> rot (-theta)
  theta = view rad ang
  rot th (coords -> x :& y :& z) = (x) ^&
                                   (cos th * y - sin th * z) ^&
                                   (sin th * y + cos th * z)

-- | Like 'aboutZ', but rotates about the Y axis, bringing postive
-- x-values towards the negative z-axis.
aboutY :: (R3Ish v) => Angle (Scalar v) -> Transformation v
aboutY ang = fromLinear r (linv r) where
  r = rot theta <-> rot (-theta)
  theta = view rad ang
  rot th (coords -> x :& y :& z) = (cos th * x + sin th * z) ^&
                                    y ^&
                                    (-sin th * x + cos th * z)

-- | @rotationAbout p d a@ is a rotation about a line parallel to @d@
--   passing through @p@.
rotationAbout :: (R3Ish v) =>
     Point v        -- ^ origin of rotation
  -> Direction v -- ^ direction of rotation axis
  -> Angle (Scalar v)     -- ^ angle of rotation
  -> Transformation v
rotationAbout p d a
  = mconcat [translation (negateV t),
             fromLinear r (linv r),
             translation t] where
    r = rot theta <-> rot (-theta)
    theta = view rad a
    w = fromDirection d
    -- rot :: Scalar v -> v -> v
    rot th v = v ^* cos th ^+^
               cross3 w v ^* sin th ^+^
               w ^* ((w <.> v) * (1 - cos th))
    t = p .-. origin

-- | @pointAt about initial final@ produces a rotation which brings
-- the direction @initial@ to point in the direction @final@ by first
-- panning around @about@, then tilting about the axis perpendicular
-- to @about@ and @final@.  In particular, if this can be accomplished
-- without tilting, it will be, otherwise if only tilting is
-- necessary, no panning will occur.  The tilt will always be between
-- ± 1/4 turn.
pointAt :: (R3Ish v) => Direction v -> Direction v -> Direction v -> Transformation v
pointAt a i f = pointAt' (fromDirection a) (fromDirection i) (fromDirection f)

-- | pointAt' has the same behavior as 'pointAt', but takes vectors
-- instead of directions.
pointAt' :: (R3Ish v) => v -> v -> v -> Transformation v
pointAt' about initial final = pointAtUnit (normalized about) (normalized initial) (normalized final)

-- | pointAtUnit has the same behavior as @pointAt@, but takes unit vectors.
pointAtUnit :: R3 -> R3 -> R3 -> T3
pointAtUnit about initial final = tilt <> pan where
  -- rotating u by (signedAngle rel u v) about rel gives a vector in the direction of v
  signedAngle rel u v = signum (cross3 u v <.> rel) *^ angleBetween u v
  inPanPlaneF = final ^-^ project about final
  inPanPlaneI = initial ^-^ project about initial
  panAngle      = signedAngle about inPanPlaneI inPanPlaneF
  pan           = rotationAbout origin (direction about) panAngle
  tiltAngle     = signedAngle tiltAxis (transform pan initial) final
  tiltAxis       = cross3 final about
  tilt          = rotationAbout origin (direction tiltAxis) tiltAngle

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the x direction.
scalingX :: (R3Ish v) => Scalar v -> Transformation v
scalingX c = fromLinear s s
  where s = (_x *~ c) <-> (_x //~ c)

-- | Scale a diagram by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use 'scale'.
scaleX :: (R3Ish v, Transformable t, V t ~ v) => Scalar v -> t -> t
scaleX = transform . scalingX

-- | Construct a transformation which scales by the given factor in
--   the y direction.
scalingY :: (R3Ish v) => Scalar v -> Transformation v
scalingY c = fromLinear s s
  where s = (_y *~ c) <-> (_y //~ c)

-- | Scale a diagram by the given factor in the y (vertical)
--   direction.  To scale uniformly, use 'scale'.
scaleY :: (R3Ish v, Transformable t, V t ~ v) => Scalar v -> t -> t
scaleY = transform . scalingY

-- | Construct a transformation which scales by the given factor in
--   the z direction.
scalingZ :: (R3Ish v) => Scalar v -> Transformation v
scalingZ c = fromLinear s s
  where s = (_z *~ c) <-> (_z //~ c)

-- | Scale a diagram by the given factor in the z direction.  To scale
-- uniformly, use 'scale'.
scaleZ :: (R3Ish v, Transformable t, V t ~ v) => Scalar v -> t -> t
scaleZ = transform . scalingZ

-- Translation ----------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the x direction.
translationX :: (R3Ish v) => Scalar v -> Transformation v
translationX x = translation (x ^& 0 ^& 0)

-- | Translate a diagram by the given distance in the x
--   direction.
translateX :: (R3Ish v, Transformable t, V t ~ v) => Scalar v -> t -> t
translateX = transform . translationX

-- | Construct a transformation which translates by the given distance
--   in the y direction.
translationY :: (R3Ish v) => Scalar v -> Transformation v
translationY y = translation (0 ^& y ^& 0)

-- | Translate a diagram by the given distance in the y
--   direction.
translateY :: (R3Ish v, Transformable t, V t ~ v) => Scalar v -> t -> t
translateY = transform . translationY

-- | Construct a transformation which translates by the given distance
--   in the z direction.
translationZ :: (R3Ish v) => Scalar v -> Transformation v
translationZ z = translation (0 ^& 0 ^& z)

-- | Translate a diagram by the given distance in the y
--   direction.
translateZ :: (R3Ish v, Transformable t, V t ~ v) => Scalar v -> t -> t
translateZ = transform . translationZ

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips a diagram across x=0,
-- i.e. sends the point (x,y,z) to (-x,y,z).
reflectionX :: (R3Ish v) => Transformation v
reflectionX = scalingX (-1)

-- | Flip a diagram across x=0, i.e. send the point (x,y,z) to (-x,y,z).
reflectX :: (R3Ish v, Transformable t, V t ~ v) => t -> t
reflectX = transform reflectionX

-- | Construct a transformation which flips a diagram across y=0,
-- i.e. sends the point (x,y,z) to (x,-y,z).
reflectionY :: (R3Ish v) => Transformation v
reflectionY = scalingY (-1)

-- | Flip a diagram across y=0, i.e. send the point (x,y,z) to
-- (x,-y,z).
reflectY :: (R3Ish v, Transformable t, V t ~ v) => t -> t
reflectY = transform reflectionY

-- | Construct a transformation which flips a diagram across z=0,
-- i.e. sends the point (x,y,z) to (x,y,-z).
reflectionZ :: (R3Ish v) => Transformation v
reflectionZ = scalingZ (-1)

-- | Flip a diagram across z=0, i.e. send the point (x,y,z) to
-- (x,y,-z).
reflectZ :: (R3Ish v, Transformable t, V t ~ v) => t -> t
reflectZ = transform reflectionZ

-- | @reflectionAbout p v@ is a reflection across the plane through
--   the point @p@ and normal to vector @v@.
reflectionAbout :: (R3Ish v) => Point v -> v -> Transformation v
reflectionAbout p v =
  conjugate (translation (origin .-. p)) reflect where
    reflect = fromLinear t (linv t)
    t = f v <-> f (negateV v)
    f u w = w ^-^ 2 *^ project u w

-- | @reflectAbout p v@ reflects a diagram in the line determined by
--   the point @p@ and the vector @v@.
reflectAbout :: (R3Ish v, Transformable t, V t ~ v) => Point v -> v -> t -> t
reflectAbout p v = transform (reflectionAbout p v)

-- Utilities ----------------------------------------

-- | Get the matrix equivalent of an affine transform, as a triple of
--   columns paired with the translation vector.  This is mostly
--   useful for implementing backends.
onBasis :: (R3Ish v) => Transformation v -> ((v, v, v), v)
onBasis t = ((x, y, z), v)
  where (x:y:z:[], v) = T.onBasis t
