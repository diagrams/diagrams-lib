{-# LANGUAGE NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Arrowtest
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Test module for Arrow.hs and Arrowheads.hs
--
-----------------------------------------------------------------------------

module Main where

import           Control.Lens                 ((%~), (&), (.~))
import           Diagrams.Backend.SVG
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude


-- | example 0 ------------------------------------------------------------
example = d # connect' (with & arrowHead .~ dart & arrowTail .~ noTail
                                  & tailSize .~ 40 & headSize .~ 40
                                  & arrowShaft .~ s & shaftStyle %~ lwO 5
                                  & headStyle %~ fc black & tailStyle %~ fc black )
                            "1" "2"
            # connect' (with & arrowHead .~ missile & headSize .~ 15 & arrowTail .~ missile'
                                   & tailSize .~ 15 & shaftStyle %~ lw 0.05 & arrowShaft .~ s1
                                   & headGap .~ 0 & tailGap .~ 0.1 )
                            "4" "3"
            # connect' (with & arrowHead .~ thorn & arrowShaft .~ a1
                                   & arrowTail .~ noTail & shaftStyle %~ lw 0.03 )
                            "1" "6"
            # connect' (with & arrowHead .~ dart & tailSize .~ 35 & arrowTail .~ dart'
                                  & headSize .~ 35 & shaftStyle %~ lw 0.1 & arrowShaft .~ s2
                                  & headGap .~ 0.1 & tailGap .~ 0.1 )
                            "4" "7"
            # connect' (with & arrowTail .~ dart' & tailSize .~ 15 & arrowShaft .~ a
                                  & arrowHead .~ spike & headSize .~ 15
                                  & shaftStyle %~ lwO 2 )
                            "9" "5"
            # connect' (with & arrowHead .~ tri & arrowTail .~ block
                             & headSize .~ 15 & tailSize .~ 15
                             & shaftStyle  %~  dashing [1,2,3,1] 0) "8" "9"
            # connectOutside "5" "8"
  where
    c = circle 1 # showOrigin # lw 0.02
    a = arc (5/12 @@ turn) (11/12 @@ turn)
    a1 = arc (1/2 @@ turn) (3/4 @@ turn)
    t = bezier3 (r2 (1,1)) (r2 (1,1)) (r2 (0,2))
    t' = reflectX t
    l = straight unitX
    l' = straight (unitX # rotateBy (1/6))
    s = trailFromSegments [t, l, t', l, t]
    s1 = cubicSpline False (trailVertices (s `at` origin))
    s2 = cubicSpline False (map p2 [(0,0), (1,0), (0.8, 0.2),(2,0.2)])
    x |-| y = x ||| strutX 2 ||| y
    row1 = (c # named "1") |-| (c # named "2") |-| (c # named "3")
    row2 = (c # named "4") |-| (c # named "5") |-| (c # named "6")
    row3 = (c # named "7") |-| (c # named "8") |-| (c # named "9")
    d = row1
         ===
        strutY 2
        ===
        row2
        ===
        strutY 2
        ===
        row3

main = defaultMain $ ( example # centerXY) # pad 1.1
