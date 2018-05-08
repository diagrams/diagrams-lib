{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
  -- for Data.Semigroup

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
       ( T3

         -- * Rotation
       , aboutX, aboutY, aboutZ
       , rotationAbout, rotateAbout
       , pointAt, pointAt'

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
       , reflectionAcross, reflectAcross

       ) where

import           Diagrams.Core
import           Diagrams.Core.Transform

import           Diagrams.Angle
import           Diagrams.Direction
import           Diagrams.Points
import           Diagrams.ThreeD.Types
import           Diagrams.Transform

import           Control.Lens            (view, (&), (*~), (.~), (//~))
import           Data.Semigroup
import           Diagrams.TwoD.Transform

import           Linear.Affine
import           Linear.Metric
import           Linear.V3               (cross)
import           Linear.Vector

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
aboutZ :: Floating n => Angle n -> Transformation V3 n
aboutZ (view rad -> a) = fromOrthogonal r where
  r = rot a <-> rot (-a)
  rot θ (V3 x y z) = V3 (cos θ * x - sin θ * y)
                        (sin θ * x + cos θ * y)
                        z

-- | Like 'aboutZ', but rotates about the X axis, bringing positive y-values
-- towards the positive z-axis.
aboutX :: Floating n => Angle n -> Transformation V3 n
aboutX (view rad -> a) = fromOrthogonal r where
  r = rot a <-> rot (-a)
  rot θ (V3 x y z) = V3 x
                        (cos θ * y - sin θ * z)
                        (sin θ * y + cos θ * z)

-- | Like 'aboutZ', but rotates about the Y axis, bringing postive
-- x-values towards the negative z-axis.
aboutY :: Floating n => Angle n -> Transformation V3 n
aboutY (view rad -> a) = fromOrthogonal r where
  r = rot a <-> rot (-a)
  rot θ (V3 x y z) = V3 (cos θ * x + sin θ * z)
                        y
                        (-sin θ * x + cos θ * z)

-- | @rotationAbout p d a@ is a rotation about a line parallel to @d@
--   passing through @p@.
rotationAbout
  :: Floating n
  => Point V3 n         -- ^ origin of rotation
  -> Direction V3 n     -- ^ direction of rotation axis
  -> Angle n            -- ^ angle of rotation
  -> Transformation V3 n
rotationAbout (P t) d (view rad -> a)
  = mconcat [translation (negated t),
             fromOrthogonal r,
             translation t] where
    r = rot a <-> rot (-a)
    w = fromDirection d

    rot θ v =          v ^* cos θ
           ^+^ cross w v ^* sin θ
           ^+^         w ^* ((w `dot` v) * (1 - cos θ))

-- | @rotationAbout p d a@ is a rotation about a line parallel to @d@
--   passing through @p@.
rotateAbout
  :: (InSpace V3 n t, Floating n, Transformable t)
  => Point V3 n         -- ^ origin of rotation
  -> Direction V3 n     -- ^ direction of rotation axis
  -> Angle n            -- ^ angle of rotation
  -> t -> t
rotateAbout p d theta = transform (rotationAbout p d theta)

-- | @pointAt about initial final@ produces a rotation which brings
-- the direction @initial@ to point in the direction @final@ by first
-- panning around @about@, then tilting about the axis perpendicular
-- to @about@ and @final@.  In particular, if this can be accomplished
-- without tilting, it will be, otherwise if only tilting is
-- necessary, no panning will occur.  The tilt will always be between
-- ± 1/4 turn.
pointAt :: (Floating n, Ord n)
        => Direction V3 n -> Direction V3 n -> Direction V3 n
        -> Transformation V3 n
pointAt a i f = pointAt' (fromDirection a) (fromDirection i) (fromDirection f)

-- | pointAt' has the same behavior as 'pointAt', but takes vectors
-- instead of directions.
pointAt' :: (Floating n, Ord n) => V3 n -> V3 n -> V3 n -> Transformation V3 n
pointAt' about initial final = pointAtUnit (signorm about) (signorm initial) (signorm final)

-- | pointAtUnit has the same behavior as @pointAt@, but takes unit vectors.
pointAtUnit :: (Floating n, Ord n) => V3 n -> V3 n -> V3 n -> Transformation V3 n
pointAtUnit about initial final = tilt <> pan where
  -- rotating u by (signedAngle rel u v) about rel gives a vector in the direction of v
  signedAngle rel u v = signum (cross u v `dot` rel) *^ angleBetween u v
  inPanPlaneF = final ^-^ project about final
  inPanPlaneI = initial ^-^ project about initial
  panAngle    = signedAngle about inPanPlaneI inPanPlaneF
  pan         = rotationAbout origin (direction about) panAngle
  tiltAngle   = signedAngle tiltAxis (transform pan initial) final
  tiltAxis    = cross final about
  tilt        = rotationAbout origin (direction tiltAxis) tiltAngle

-- Scaling -------------------------------------------------

-- | Construct a transformation which scales by the given factor in
--   the z direction.
scalingZ :: (Additive v, R3 v, Fractional n) => n -> Transformation v n
scalingZ c = fromSymmetric $ (_z *~ c) <-> (_z //~ c)

-- | Scale a diagram by the given factor in the z direction.  To scale
-- uniformly, use 'scale'.
scaleZ :: (InSpace v n t, R3 v, Fractional n, Transformable t) => n -> t -> t
scaleZ = transform . scalingZ

-- Translation ----------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the z direction.
translationZ :: (Additive v, R3 v, Num n) => n -> Transformation v n
translationZ z = translation (zero & _z .~ z)

-- | Translate a diagram by the given distance in the y
--   direction.
translateZ :: (InSpace v n t, R3 v, Transformable t) => n -> t -> t
translateZ = transform . translationZ

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips a diagram across z=0,
-- i.e. sends the point (x,y,z) to (x,y,-z).
reflectionZ :: (Additive v, R3 v, Num n) => Transformation v n
reflectionZ = fromSymmetric $ (_z *~ (-1)) <-> (_z *~ (-1))

-- | Flip a diagram across z=0, i.e. send the point (x,y,z) to
-- (x,y,-z).
reflectZ :: (InSpace v n t, R3 v, Transformable t) => t -> t
reflectZ = transform reflectionZ

-- | @reflectionAcross p v@ is a reflection across the plane through
--   the point @p@ and normal to vector @v@. This also works as a 2D
--   transform where @v@ is the normal to the line passing through point
--   @p@.
reflectionAcross :: (Metric v, Fractional n)
  => Point v n -> v n -> Transformation v n
reflectionAcross p v =
  conjugate (translation (origin .-. p)) reflect
    where
      reflect = fromLinear t (linv t)
      t       = f v <-> f (negated v)
      f u w   = w ^-^ 2 *^ project u w

-- | @reflectAcross p v@ reflects a diagram across the plane though
--   the point @p@ and the vector @v@. This also works as a 2D transform
--   where @v@ is the normal to the line passing through point @p@.
reflectAcross :: (InSpace v n t, Metric v, Fractional n, Transformable t)
  => Point v n -> v n -> t -> t
reflectAcross p v = transform (reflectionAcross p v)
