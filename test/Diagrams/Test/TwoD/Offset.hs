module Diagrams.Test.TwoD.Offset
    (
      tests
    ) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import Diagrams.Prelude
import Diagrams.TwoD.Offset

tests :: [TestTree]
tests =
    [ testCase "line"
          (offsetTrailVertices
              [p2 (0, 0), p2 (1, 0)]
              [p2 (0, -1), p2 (1, -1)])
    , testCase "square"
          (offsetTrailVertices
              [p2 (0, 0), p2 (1, 0), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
              [p2 (0, -1), p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, 0)])
    , testCase "square loop"
          (offsetTrailLoopVertices
              [p2 (0, 0), p2 (1, 0), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
              [p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, -1)])
    , testCase "redundant line"
          (offsetTrailVertices
              [p2 (0, 0), p2 (0.5, 0), p2 (1, 0)]
              [p2 (0, -1), p2 (1, -1)])
    , testCase "redundant square"
          (offsetTrailVertices
              [p2 (0, 0), p2 (1, 0), p2 (1, 0.5), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
              [p2 (0, -1), p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, 0)])
    , testCase "redundant square loop"
          (offsetTrailLoopVertices
              [p2 (0, 0), p2 (1, 0), p2 (1, 0.5), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
              [p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, -1)])
    ]

offsetTrailVertices :: [Point V2 Double] -> [Point V2 Double] -> Assertion
offsetTrailVertices orig off =
    (trailVertices . offsetTrail 1 . fromVertices $ orig) @?= off

offsetTrailLoopVertices :: [Point V2 Double] -> [Point V2 Double] -> Assertion
offsetTrailLoopVertices orig off =
    (trailVertices . offsetTrail 1 . loopTrailFromVertices $ orig) @?= off
  where
    loopTrailFromVertices = (`at` origin) . wrapTrail . glueLine . lineFromVertices
