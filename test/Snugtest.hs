{-# LANGUAGE NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Arrowtest
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Test module for Diagrams.Snug and Diagrams.TwoD.Snug
--
-----------------------------------------------------------------------------

module Main where

import           Diagrams.Backend.SVG
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (alignL, alignR, centerXY)

import           Diagrams.Align
import           Diagrams.TwoD.Align

concave :: Diagram SVG R2
concave = polygon (with & polyType .~ PolyPolar [a, b, b, b]
               [0.25,1,1,1,1] & polyOrient .~ NoOrient)
               # fc blue # lw 0
  where
    a = 1/8 :: Turn
    b = 1/4 :: Turn

convex :: Diagram SVG R2
convex = polygon (with & polyType .~ PolyPolar [a,b] [0.25, 1, 1]
                       & polyOrient .~ NoOrient)
                      # fc orange # lw 0
  where
    a = 1/8 :: Turn
    b = 3/4 :: Turn

example1 = (concave # centerXY # alignR # showOrigin)
        <> (convex # centerXY # alignL # showOrigin)

example2 = (concave # centerXY # snugR # showOrigin)
        <> (convex # centerXY # snugL # showOrigin)

example3 = (concave # rotateBy (1/4 :: Turn) # centerXY
                    # snugT # showOrigin)
        <> (convex # rotateBy (1/4 :: Turn) # centerXY
                   #snugB # showOrigin)
example4= (concave # rotateBy (2/4 :: Turn) # centerXY
                  # snugL # showOrigin)
       <> (convex # rotateBy (2/4 :: Turn) # centerXY
                  # snugR # showOrigin)
example5= (concave # rotateBy (3/4 :: Turn) # centerXY
                  # snugB # showOrigin)
       <> (convex # rotateBy (3/4 :: Turn) # centerXY
                  # snugT # showOrigin)
example6 = (mconcat $ [circle 0.25 # fc orange, concave] # alignR) # showOrigin
example7 = (mconcat $ [circle 0.25 # fc orange, concave] # snugR) # showOrigin

main = defaultMain $ ( example1 # centerXY
                    === strutY 0.25
                    === example2 # centerXY
                    === strutY 0.25
                    === example3 # centerXY
                    === strutY 0.25
                    === example4 # centerXY
                    === strutY 0.25
                    === example5 # centerXY
                    === strutY 0.25
                    === example6 # centerXY
                    === strutY 0.25
                    === example7 # centerXY
                    ) # pad 1.1
