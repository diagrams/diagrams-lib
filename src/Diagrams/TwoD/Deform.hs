module Diagrams.TwoD.Deform where

import Control.Lens

import Diagrams.Deform
import Diagrams.TwoD.Types

-- | The parallel projection onto the line x=0
parallelX0 :: Floating n => Deformation V2 n
parallelX0 = Deformation (& _x .~ 0)

-- | The perspective division onto the line x=1 along lines going
-- through the origin.
perspectiveX1 :: Floating n => Deformation V2 n
perspectiveX1 = Deformation (\p -> p & _y //~ (p^._x) & _x .~ 1)

-- | The parallel projection onto the line y=0
parallelY0 :: Floating n => Deformation V2 n
parallelY0 = Deformation (& _y .~ 0)

-- | The perspective division onto the line y=1 along lines going
-- through the origin.
perspectiveY1 :: Floating n => Deformation V2 n
perspectiveY1 = Deformation (\p -> p & _x //~ (p^._y) & _y .~ 1)

-- | The viewing transform for a viewer facing along the positive X
-- axis.  X coördinates stay fixed, while Y coördinates are compressed
-- with increasing distance.  @asDeformation (translation unitX) <>
-- parallelX0 <> frustrumX = perspectiveX1@
facingX :: Floating n => Deformation V2 n
facingX = Deformation (\v -> v & _y //~ (v^._x))

facingY :: Floating n => Deformation V2 n
facingY = Deformation (\v -> v & _x //~ (v^._y))

