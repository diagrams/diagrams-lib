{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
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
aboutZ :: Angle -> T3
aboutZ ang = fromLinear r (linv r) where
  r = rot theta <-> rot (-theta)
  theta = view rad ang
  rot th (coords -> x :& y :& z) = (cos th * x - sin th * y) ^&
                                   (sin th * x + cos th * y) ^&
                                   z

-- | Like 'aboutZ', but rotates about the X axis, bringing positive y-values
-- towards the positive z-axis.
aboutX :: Angle -> T3
aboutX ang = fromLinear r (linv r) where
  r = rot theta <-> rot (-theta)
  theta = view rad ang
  rot th (coords -> x :& y :& z) = (x) ^&
                                   (cos th * y - sin th * z) ^&
                                   (sin th * y + cos th * z)

-- | Like 'aboutZ', but rotates about the Y axis, bringing postive
-- x-values towards the negative z-axis.
aboutY :: Angle -> T3
aboutY ang = fromLinear r (linv r) where
  r = rot theta <-> rot (-theta)
  theta = view rad ang
  rot th (coords -> x :& y :& z) = (cos th * x + sin th * z) ^&
                                    y ^&
                                    (-sin th * x + cos th * z)

-- | @rotationAbout p d a@ is a rotation about a line parallel to @d@
--   passing through @p@.
rotationAbout ::
     P3        -- ^ origin of rotation
  -> Direction R3 -- ^ direction of rotation axis
  -> Angle     -- ^ angle of rotation
  -> T3
rotationAbout p d a
  = mconcat [translation (negateV t),
             fromLinear r (linv r),
             translation t] where
    r = rot theta <-> rot (-theta)
    theta = view rad a
    w = fromDirection d
    rot :: Double -> R3 -> R3
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
pointAt :: Direction R3 -> Direction R3 -> Direction R3 -> T3
pointAt a i f = pointAt' (fromDirection a) (fromDirection i) (fromDirection f)

-- | pointAt' has the same behavior as 'pointAt', but takes vectors
-- instead of directions.
pointAt' :: R3 -> R3 -> R3 -> T3
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
scalingX :: Double -> T3
scalingX c = fromLinear s s
  where s = (_x *~ c) <-> (_x //~ c)

-- | Scale a diagram by the given factor in the x (horizontal)
--   direction.  To scale uniformly, use 'scale'.
scaleX :: (Transformable t, V t ~ R3) => Double -> t -> t
scaleX = transform . scalingX

-- | Construct a transformation which scales by the given factor in
--   the y direction.
scalingY :: Double -> T3
scalingY c = fromLinear s s
  where s = (_y *~ c) <-> (_y //~ c)

-- | Scale a diagram by the given factor in the y (vertical)
--   direction.  To scale uniformly, use 'scale'.
scaleY :: (Transformable t, V t ~ R3) => Double -> t -> t
scaleY = transform . scalingY

-- | Construct a transformation which scales by the given factor in
--   the z direction.
scalingZ :: Double -> T3
scalingZ c = fromLinear s s
  where s = (_z *~ c) <-> (_z //~ c)

-- | Scale a diagram by the given factor in the z direction.  To scale
-- uniformly, use 'scale'.
scaleZ :: (Transformable t, V t ~ R3) => Double -> t -> t
scaleZ = transform . scalingZ

-- Translation ----------------------------------------

-- | Construct a transformation which translates by the given distance
--   in the x direction.
translationX :: Double -> T3
translationX x = translation (x ^& 0 ^& 0)

-- | Translate a diagram by the given distance in the x
--   direction.
translateX :: (Transformable t, V t ~ R3) => Double -> t -> t
translateX = transform . translationX

-- | Construct a transformation which translates by the given distance
--   in the y direction.
translationY :: Double -> T3
translationY y = translation (0 ^& y ^& 0)

-- | Translate a diagram by the given distance in the y
--   direction.
translateY :: (Transformable t, V t ~ R3) => Double -> t -> t
translateY = transform . translationY

-- | Construct a transformation which translates by the given distance
--   in the z direction.
translationZ :: Double -> T3
translationZ z = translation (0 ^& 0 ^& z)

-- | Translate a diagram by the given distance in the y
--   direction.
translateZ :: (Transformable t, V t ~ R3) => Double -> t -> t
translateZ = transform . translationZ

-- Reflection ----------------------------------------------

-- | Construct a transformation which flips a diagram across x=0,
-- i.e. sends the point (x,y,z) to (-x,y,z).
reflectionX :: T3
reflectionX = scalingX (-1)

-- | Flip a diagram across x=0, i.e. send the point (x,y,z) to (-x,y,z).
reflectX :: (Transformable t, V t ~ R3) => t -> t
reflectX = transform reflectionX

-- | Construct a transformation which flips a diagram across y=0,
-- i.e. sends the point (x,y,z) to (x,-y,z).
reflectionY :: T3
reflectionY = scalingY (-1)

-- | Flip a diagram across y=0, i.e. send the point (x,y,z) to
-- (x,-y,z).
reflectY :: (Transformable t, V t ~ R3) => t -> t
reflectY = transform reflectionY

-- | Construct a transformation which flips a diagram across z=0,
-- i.e. sends the point (x,y,z) to (x,y,-z).
reflectionZ :: T3
reflectionZ = scalingZ (-1)

-- | Flip a diagram across z=0, i.e. send the point (x,y,z) to
-- (x,y,-z).
reflectZ :: (Transformable t, V t ~ R3) => t -> t
reflectZ = transform reflectionZ

-- | @reflectionAbout p v@ is a reflection across the plane through
--   the point @p@ and normal to vector @v@.
reflectionAbout :: P3 -> R3 -> T3
reflectionAbout p v =
  conjugate (translation (origin .-. p)) reflect where
    reflect = fromLinear t (linv t)
    t = f v <-> f (negateV v)
    f u w = w ^-^ 2 *^ project u w

-- | @reflectAbout p v@ reflects a diagram in the line determined by
--   the point @p@ and the vector @v@.
reflectAbout :: (Transformable t, V t ~ R3) => P3 -> R3 -> t -> t
reflectAbout p v = transform (reflectionAbout p v)

-- Utilities ----------------------------------------

-- | Get the matrix equivalent of an affine transform, as a triple of
--   columns paired with the translation vector.  This is mostly
--   useful for implementing backends.
onBasis :: T3 -> ((R3, R3, R3), R3)
onBasis t = ((x, y, z), v)
  where (x:y:z:[], v) = T.onBasis t
