module Diagrams.TwoD.Deform where

import Control.Lens

import Diagrams.Core.Deform

import Diagrams.Coordinates
import Diagrams.TwoD.Types

-- | The parallel projection onto the line x=0
parallelX0 :: Deformation R2
parallelX0 = Deformation (& _x .~ 0)

-- | The perspective division onto the line x=1 along lines going
-- through the origin.
perspectiveX1 :: Deformation R2
perspectiveX1 = Deformation (\p -> p & _y //~ (p^._x) & _x .~ 1)

-- | The parallel projection onto the line y=0
parallelY0 :: Deformation R2
parallelY0 = Deformation (& _y .~ 0)

-- | The perspective division onto the line y=1 along lines going
-- through the origin.
perspectiveY1 :: Deformation R2
perspectiveY1 = Deformation (\p -> p & _x //~ (p^._y) & _y .~ 1)

-- | The viewing transform for a viewer facing along the positive X
-- axis.  X coördinates stay fixed, while Y coördinates are compressed
-- with increasing distance.  @asDeformation (translation unitX) <>
-- parallelX0 <> frustrumX = perspectiveX1@
facingX :: Deformation R2
facingX = Deformation (\v -> v & _y //~ (v^._x))

facingY :: Deformation R2
facingY = Deformation (\v -> v & _x //~ (v^._y))
