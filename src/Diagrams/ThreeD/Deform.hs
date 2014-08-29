module Diagrams.ThreeD.Deform
  ( parallelX0, perspectiveX1, facingX
  , parallelY0, perspectiveY1, facingY
  , parallelZ0, perspectiveZ1, facingZ
  ) where

import Control.Lens

import Diagrams.Deform
import Diagrams.TwoD.Deform

import Linear.Vector
import Linear.V3

-- | The parallel projection onto the plane z=0
parallelZ0 :: (R3 v, Num n) => Deformation v n
parallelZ0 = Deformation (_z .~ 0)

-- | The perspective division onto the plane z=1 along lines going
--   through the origin.
perspectiveZ1 :: (R3 v, Functor v, Fractional n) => Deformation v n
perspectiveZ1 = Deformation $ \p -> p ^/ (p ^. _x)

facingZ :: (R3 v, Functor v, Fractional n) => Deformation v n
facingZ = Deformation $
  \p -> let z = p ^. _z
        in  p ^/ z & _z .~ z

