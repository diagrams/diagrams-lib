import Test.Tasty (defaultMain, testGroup, TestTree)

import qualified Diagrams.TwoD.OffsetTest as TwoD.OffsetTest

tests :: TestTree
tests = testGroup "unit tests"
    [ testGroup "TwoD.Offset" TwoD.OffsetTest.tests ]

main :: IO ()
main = defaultMain tests
