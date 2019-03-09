{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module Diagrams.Test.TwoD.Segment
    (
      tests
    ) where

import           Test.Tasty (TestTree)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import qualified Test.QuickCheck.Property as Q

import           Diagrams.Prelude
import           Diagrams.TwoD.Segment

newtype InBox = InBox { unInBox :: Double }

instance Arbitrary InBox where
  arbitrary = InBox <$> choose (-1, 1)

instance Arbitrary (Point V2 Double) where
  arbitrary = curry p2 <$> (unInBox <$> arbitrary)
                       <*> (unInBox <$> arbitrary)

instance Arbitrary (FixedSegment V2 Double) where
   arbitrary = oneof [FLinear <$> arbitrary <*> arbitrary, FCubic <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary]

epsT = 1.0e-9 -- parameter space epsilon
epsE = 1.0e-8 -- Euclidean space epsilon

x .=~. y = norm (x .-. y) < epsE

tests :: [TestTree]
tests =
    [ testProperty "segmentSegment" $
        \a b -> validateIntersections a b (segmentSegment epsT a b)
    ]

validateIntersections :: FixedSegment V2 Double -> FixedSegment V2 Double -> [(Double, Double, P2 Double)] -> Q.Result
validateIntersections a b [] = Q.rejected -- TODO: check for false negatives (rasterize both and look for overlap?)
validateIntersections a b is = go is
  where
    go [] = Q.succeeded
    go ((ta,tb,p):is)
      | and [ 0 <= ta && ta <= 1
            , 0 <= tb && tb <= 1
            , a `atParam` ta .=~. p
            , b `atParam` tb .=~. p
            ] = go is
      | otherwise = Q.failed
