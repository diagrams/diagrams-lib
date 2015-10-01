import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import qualified Diagrams.TwoD.OffsetTest as TwoD.OffsetTest

tests :: [Test]
tests = [ testGroup "TwoD.Offset" $ hUnitTestToTests TwoD.OffsetTest.tests ]

main :: IO ()
main = defaultMain tests
