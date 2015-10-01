module Diagrams.TwoD.OffsetTest
    (
      tests
    ) where

import Test.HUnit

import Diagrams.Prelude
import Diagrams.TwoD.Offset

tests :: Test
tests = test [ "line" ~: offsetTrailVertices
                             [p2 (0, 0), p2 (1, 0)]
                             [p2 (0, -1), p2 (1, -1)]
             , "square" ~: offsetTrailVertices
                             [p2 (0, 0), p2 (1, 0), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
                             [p2 (0, -1), p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, 0)]
             , "square loop" ~: offsetTrailLoopVertices
                             [p2 (0, 0), p2 (1, 0), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
                             [p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, -1)]
             , "redundant line" ~: offsetTrailVertices
                             [p2 (0, 0), p2 (0.5, 0), p2 (1, 0)]
                             [p2 (0, -1), p2 (1, -1)]
             , "redundant square" ~: offsetTrailVertices
                             [p2 (0, 0), p2 (1, 0), p2 (1, 0.5), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
                             [p2 (0, -1), p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, 0)]
             , "redundant square loop" ~: offsetTrailLoopVertices
                             [p2 (0, 0), p2 (1, 0), p2 (1, 0.5), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
                             [p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, -1)]
             ]

offsetTrailVertices :: [Point V2 Double] -> [Point V2 Double] -> Test
offsetTrailVertices orig off =
    (trailVertices . offsetTrail 1 . fromVertices $ orig) ~?= off

offsetTrailLoopVertices :: [Point V2 Double] -> [Point V2 Double] -> Test
offsetTrailLoopVertices orig off =
    (trailVertices . offsetTrail 1 . loopTrailFromVertices $ orig) ~?= off
  where
    loopTrailFromVertices = (`at` origin) . wrapTrail . glueLine . lineFromVertices
