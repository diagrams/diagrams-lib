module Diagrams.TwoD.Project where

import Control.Lens

import Diagrams.Core.Project

import Diagrams.Coordinates
import Diagrams.TwoD.Types

-- | The parallel projection onto the line x=0
parallelX0 :: Projection R2
parallelX0 = Projection (& _x .~ 0)

-- | The perspective division onto the line x=1 along lines going
-- through the origin.
perspectiveX1 :: Projection R2
perspectiveX1 = Projection (\p -> p & _y //~ (p^._x) & _x .~ 1)

-- | The parallel projection onto the line y=0
parallelY0 :: Projection R2
parallelY0 = Projection (& _y .~ 0)

-- | The perspective division onto the line y=1 along lines going
-- through the origin.
perspectiveY1 :: Projection R2
perspectiveY1 = Projection (\p -> p & _x //~ (p^._y) & _y .~ 1)

-- | The viewing transform for a viewer facing along the positive X
-- axis.  X coördinates stay fixed, while Y coördinates are compressed
-- with increasing distance.  @asProjection (translation unitX) <>
-- parallelX0 <> frustrumX = perspectiveX1@
facingX :: Projection R2
facingX = Projection (\v -> v & _y //~ (v^._x))

facingY :: Projection R2
facingY = Projection (\v -> v & _x //~ (v^._y))
