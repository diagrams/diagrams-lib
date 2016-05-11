-- |

module Diagrams.Test.Direction where


import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Diagrams.Prelude
import           Diagrams.Direction
import Instances

tests :: TestTree
tests = testGroup "Direction" [
         testProperty "Length does not effect from direction" $
           \(Positive f) (NonZero v) -> fromDirection(dir ((v  :: V2 Double) ^* (f+0.001))) =~ fromDirection(dir v)
         , testProperty "HasTheta subtraction yeilds same result as anglebetween" $
            (anglebetsub)
         , testProperty "anglebetweenDirs is commutative" $
           \a b -> angleBetweenDirs (a :: Direction V2 Float) b =~ angleBetweenDirs b a
         , testProperty "fromdirection does not effect angleBetweenDirs" $
           \a b -> angleBetween (fromDirection (a  :: Direction V2 Float)) (fromDirection b) =~ angleBetweenDirs a b



      ]

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

anglebetsub :: Direction V2 Float -> Direction V2 Float -> Bool
anglebetsub a b = (if' (abs (a ^.  _theta^.rad  - b ^. _theta^.rad) < pi)
                       (abs ((a ^.  _theta  ^-^ b ^. _theta)^.rad))
                       (2*pi - abs (a ^.  _theta^.rad  - b ^. _theta^.rad) ) =~ angleBetweenDirs a b ^.rad)

-- anglebetsub a b = ((a ^.  _theta^.rad  - b ^. _theta^.rad)  =~ angleBetweenDirs a b ^.rad)

-- if (a /= b)  (angleBetweenDirs a b ^.rad) ((a ^.  _theta  ^-^ b ^. _theta) ^.rad)
      -- \a b -> (mod (abs (((a :: Direction V2 Float) ^.  _theta  ^-^ b ^. _theta) ^.rad)) (pi/2)) ^+^ (0.5 + 0.5* signum (abs ((a^.  _theta  ^-^ b ^. _theta) ^.rad) - pi/2)) * pi/2  =~ angleBetweenDirs a b ^.rad
