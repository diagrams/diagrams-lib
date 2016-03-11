import Test.Tasty (defaultMain, testGroup, TestTree)

import qualified Diagrams.Test.TwoD as TwoD
import qualified Diagrams.Test.TwoD.Offset as TwoD.Offset
import qualified Diagrams.Test.Angle as Angle

tests :: TestTree
tests = testGroup "unit tests"
    [ testGroup "TwoD.Offset" TwoD.Offset.tests
    , TwoD.tests
    , Angle.tests
    ]

main :: IO ()
main = defaultMain tests
