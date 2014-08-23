module Diagrams.ThreeD.Deform where

import Control.Lens

import Diagrams.Deform
import Diagrams.ThreeD.Types

-- | The parallel projection onto the plane x=0
parallelX0 :: Floating n => Deformation V3 n
parallelX0 = Deformation (& _x .~ 0)

-- | The perspective division onto the plane x=1 along lines going
-- through the origin.
perspectiveX1 :: Floating n => Deformation V3 n
perspectiveX1 = Deformation (\p -> let x = p^._x in
                             p & _x .~ 1 & _y //~ x & _z //~ x )

-- | The parallel projection onto the plane y=0
parallelY0 :: Floating n => Deformation V3 n
parallelY0 = Deformation (& _y .~ 0)

-- | The perspective division onto the plane y=1 along lines going
-- through the origin.
perspectiveY1 :: Floating n => Deformation V3 n
perspectiveY1 = Deformation (\p -> let y = p^._y in
                             p & _x //~ y & _y .~ 1 & _z //~ y )

-- | The parallel projection onto the plane z=0
parallelZ0 :: Floating n => Deformation V3 n
parallelZ0 = Deformation (& _z .~ 0)

-- | The perspective division onto the plane z=1 along lines going
-- through the origin.
perspectiveZ1 :: Floating n => Deformation V3 n
perspectiveZ1 = Deformation (\p -> let z = p^._z in
                             p & _x //~ z & _y //~ z & _z .~ 1 )

-- | The viewing transform for a viewer facing along the positive X
-- axis.  X coördinates stay fixed, while Y coördinates are compressed
-- with increasing distance.  @asDeformation (translation unitX) <>
-- parallelX0 <> frustrumX = perspectiveX1@
facingX :: Floating n => Deformation V3 n
facingX = Deformation (\v -> v & _y //~ (v^._x) & _z //~ (v^._x))

facingY :: Floating n => Deformation V3 n
facingY = Deformation (\v -> v & _x //~ (v^._y) & _z //~ (v^._y))

facingZ :: Floating n => Deformation V3 n
facingZ = Deformation (\v -> v & _x //~ (v^._z) & _y //~ (v^._z))

