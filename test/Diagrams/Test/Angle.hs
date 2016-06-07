-- |

module Diagrams.Test.Angle where


import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Diagrams.Prelude
import Instances

tests :: TestTree
tests = testGroup "Angle" [
         testProperty "2π radians per turn" $
           \θ -> θ^.rad =~ θ^.turn*2*(pi :: Double)
         , testProperty "360 degrees per turn" $
           \θ -> θ^.deg =~ θ^.turn*(360 :: Double)
         , testProperty "Angle vector addition is commutative" $
           \θ φ -> (θ :: Angle Double) ^+^ φ =~ φ ^+^ θ
         , testProperty "Angle subtraction is the inverse of addition" $
           \θ φ -> (θ :: Angle Double) ^+^ φ ^-^ φ =~ θ
         , testProperty "Angle vector negation squared is identity" $
           \θ -> negated (negated (θ :: Angle Double)) =~ θ
         , testProperty "A negated angle is the additive inverse of the original" $
           \θ -> (θ :: Angle Double) ^+^ (negated θ) =~ 0@@turn
         , testProperty "A negated angle is the additive inverse of the original" $
           \θ -> (θ :: Angle Double) ^+^ (negated θ) =~ 0@@turn


      ]
