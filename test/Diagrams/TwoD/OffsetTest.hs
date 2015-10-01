module Diagrams.TwoD.OffsetTest
    (
      tests
    ) where

import Test.HUnit

import Diagrams.Prelude
import Diagrams.TwoD.Offset

tests :: Test
tests = test [ "line" ~: offsetPathVertices
                             [p2 (0, 0), p2 (1, 0)]
                             [p2 (0, -1), p2 (1, -1)]
             , "square" ~: offsetPathVertices
                             [p2 (0, 0), p2 (1, 0), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
                             [p2 (0, -1), p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, 0)]
             , "square loop" ~: offsetPathLoopVertices
                             [p2 (0, 0), p2 (1, 0), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
                             [p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, -1)]
             , "redundant line" ~: offsetPathVertices
                             [p2 (0, 0), p2 (0.5, 0), p2 (1, 0)]
                             [p2 (0, -1), p2 (1, -1)]
             , "redundant square" ~: offsetPathVertices
                             [p2 (0, 0), p2 (1, 0), p2 (1, 0.5), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
                             [p2 (0, -1), p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, 0)]
             , "redundant square loop" ~: offsetPathLoopVertices
                             [p2 (0, 0), p2 (1, 0), p2 (1, 0.5), p2 (1, 1), p2 (0, 1), p2 (0, 0)]
                             [p2 (2, -1), p2 (2, 2), p2 (-1, 2), p2 (-1, -1)]
             ]

offsetPathVertices :: [Point V2 Double] -> [Point V2 Double] -> Test
offsetPathVertices orig offset =
    (concat . pathVertices . offsetPath 1 . fromVertices $ orig) ~?= offset

offsetPathLoopVertices :: [Point V2 Double] -> [Point V2 Double] -> Test
offsetPathLoopVertices orig offset =
    (concat . pathVertices . offsetPath 1 . loopPathFromVertices $ orig) ~?= offset
  where
    loopPathFromVertices = pathFromTrail . wrapTrail . glueLine . lineFromVertices
