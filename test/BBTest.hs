import Test.QuickCheck

import Diagrams.BoundingBox

instance Arbitrary (NonEmptyBoundingBox Q2) where
  arbitrary = do
    p <- arbitrary
    PosVec v <- arbitrary
    return $ NonEmptyBoundingBox p (p .+^ v)