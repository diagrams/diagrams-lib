module Diagrams.TwoD.Deform where

import Control.Lens

import Diagrams.Deform

import Linear.V2
import Linear.Vector

-- | The parallel projection onto the plane x=0
parallelX0 :: (R1 v, Num n) => Deformation v n
parallelX0 = Deformation (_x .~ 0)

-- | The perspective division onto the plane x=1 along lines going
-- through the origin.
perspectiveX1 :: (R1 v, Functor v, Fractional n) => Deformation v n
perspectiveX1 = Deformation $ \p -> p ^/ (p ^. _x)

-- | The parallel projection onto the plane y=0
parallelY0 :: (R2 v, Num n) => Deformation v n
parallelY0 = Deformation (_y .~ 0)

-- | The perspective division onto the plane y=1 along lines going
-- through the origin.
perspectiveY1 :: (R2 v, Functor v, Floating n) => Deformation v n
perspectiveY1 = Deformation $ \p -> p ^/ (p ^. _y)

-- | The viewing transform for a viewer facing along the positive X
-- axis.  X coördinates stay fixed, while Y coördinates are compressed
-- with increasing distance.  @asDeformation (translation unitX) <>
-- parallelX0 <> frustrumX = perspectiveX1@
facingX :: (R1 v, Functor v, Fractional n) => Deformation v n
facingX = Deformation $
  \p -> let x = p ^. _x
        in  p ^/ x & _x .~ x

facingY :: (R2 v, Functor v, Fractional n) => Deformation v n
facingY = Deformation $
  \p -> let y = p ^. _y
        in  p ^/ y & _y .~ y

