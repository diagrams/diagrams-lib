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

module Diagrams.ThreeD.Transform where

import           Diagrams.Core
import qualified Diagrams.Core.Transform as T

import           Diagrams.ThreeD.Types
import           Diagrams.ThreeD.Vector
import           Diagrams.Coordinates

import           Data.Semigroup

import           Data.AffineSpace
import           Data.Cross
import           Data.VectorSpace

-- | Create a transformation which rotates by the given angle about
--   a line parallel the Z axis passing through the local origin.
--   A positive angle brings positive x-values towards the positive-y axis.
--
--   The angle can be expressed using any type which is an
--   instance of 'Angle'.  For example, @aboutZ (1\/4 ::
--   'Turn')@, @aboutZ (tau\/4 :: 'Rad')@, and @aboutZ (90 ::
--   'Deg')@ all represent the same transformation, namely, a
--   counterclockwise rotation by a right angle.  For more general rotations,
--   see 'rotationAbout'.
--
--   Note that writing @aboutZ (1\/4)@, with no type annotation, will
--   yield an error since GHC cannot figure out which sort of angle
--   you want to use.
aboutZ :: Angle a => a -> T3
aboutZ ang = fromLinear r (linv r) where
  r = rot theta <-> rot (-theta)
  Rad theta = convertAngle ang
  rot th (coords -> x :& y :& z) = (cos th * x - sin th * y) @@
                                   (sin th * x + cos th * y) @@
                                   z

-- | Like 'aboutZ', but rotates about the X axis, bringing positive y-values
-- towards the positive z-axis.
aboutX :: Angle a => a -> T3
aboutX ang = fromLinear r (linv r) where
  r = rot theta <-> rot (-theta)
  Rad theta = convertAngle ang
  rot th (coords -> x :& y :& z) = (x) @@
                                   (cos th * y - sin th * z) @@
                                   (sin th * y + cos th * z)

-- | Like 'aboutZ', but rotates about the Y axis, bringing postive
-- x-values towards the negative z-axis.
aboutY :: Angle a => a -> T3
aboutY ang = fromLinear r (linv r) where
  r = rot theta <-> rot (-theta)
  Rad theta = convertAngle ang
  rot th (coords -> x :& y :& z) = (cos th * x + sin th * z) @@
                                    y @@
                                    (-sin th * x + cos th * z)

-- | @rotationAbout p d a@ is a rotation about a line parallel to @d@
--   passing through @p@.
rotatationAbout
  :: (Angle a, Direction d)
  => P3     -- ^ origin of rotation
  -> d      -- ^ direction of rotation axis
  -> a      -- ^ angle of rotation
  -> T3
rotatationAbout p d a
  = mconcat [translation (negateV t),
             fromLinear r (linv r),
             translation t] where
    r = rot theta <-> rot (-theta)
    Rad theta = convertAngle a
    w = fromDirection d
    rot :: Double -> R3 -> R3
    rot th v = v ^* cos th ^+^
               cross3 w v ^* sin th ^+^
               w ^* ((w <.> v) * (1 - cos th))
    t = p .-. origin

-- | Get the matrix equivalent of an affine transform, as a triple of
--   columns paired with the translation vector.  This is mostly
--   useful for implementing backends.
onBasis :: T3 -> ((R3, R3, R3), R3)
onBasis t = ((x, y, z), v)
  where ((x:y:z:[]), v) = T.onBasis t
