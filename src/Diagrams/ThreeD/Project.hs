module Diagrams.ThreeD.Project where

import Control.Lens

import Diagrams.Core.Project

import Diagrams.Coordinates
import Diagrams.ThreeD.Types

-- | The parallel projection onto the plane x=0
parallelX0 :: Projection R3
parallelX0 = Projection (& _x .~ 0)

-- | The perspective division onto the plane x=1 along lines going
-- through the origin.
perspectiveX1 :: Projection R3
perspectiveX1 = Projection (\p -> let x = p^._x in
                             p & _x .~ 1 & _y //~ x & _z //~ x )

-- | The parallel projection onto the plane y=0
parallelY0 :: Projection R3
parallelY0 = Projection (& _y .~ 0)

-- | The perspective division onto the plane y=1 along lines going
-- through the origin.
perspectiveY1 :: Projection R3
perspectiveY1 = Projection (\p -> let y = p^._y in
                             p & _x //~ y & _y .~ 1 & _z //~ y )

-- | The parallel projection onto the plane z=0
parallelZ0 :: Projection R3
parallelZ0 = Projection (& _z .~ 0)

-- | The perspective division onto the plane z=1 along lines going
-- through the origin.
perspectiveZ1 :: Projection R3
perspectiveZ1 = Projection (\p -> let z = p^._z in
                             p & _x //~ z & _y //~ z & _z .~ 1 )

-- | The viewing transform for a viewer facing along the positive X
-- axis.  X coördinates stay fixed, while Y coördinates are compressed
-- with increasing distance.  @asProjection (translation unitX) <>
-- parallelX0 <> frustrumX = perspectiveX1@
facingX :: Projection R3
facingX = Projection (\v -> v & _y //~ (v^._x) & _z //~ (v^._x))

facingY :: Projection R3
facingY = Projection (\v -> v & _x //~ (v^._y) & _z //~ (v^._y))

facingZ :: Projection R3
facingZ = Projection (\v -> v & _x //~ (v^._z) & _y //~ (v^._z))
