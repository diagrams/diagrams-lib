import           Test.Tasty                (TestTree, defaultMain, testGroup)

import qualified Diagrams.Test.Angle            as Angle
import qualified Diagrams.Test.Direction        as Direction
import qualified Diagrams.Test.Transform        as Transform
import qualified Diagrams.Test.Transform.Matrix as TransformMatrix
import qualified Diagrams.Test.TwoD             as TwoD
import qualified Diagrams.Test.TwoD.Offset      as TwoD.Offset
import qualified Diagrams.Test.TwoD.Segment     as TwoD.Segment

import qualified Diagrams.Test.Trail            as Trail

tests :: TestTree
tests = testGroup "unit tests"
    [ testGroup "TwoD.Offset" TwoD.Offset.tests
    , testGroup "TwoD.Segment" TwoD.Segment.tests
    , TwoD.tests
    , Angle.tests
    , Direction.tests
    , Transform.tests
    , TransformMatrix.tests
    , Trail.tests
    ]

main :: IO ()
main = defaultMain tests
