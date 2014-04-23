{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Diagrams.Prelude
import           Diagrams.Backend.Rasterific.CmdLine

radial = mkRadialGradient (mkStops [(white,0,1), (black,1,1)])  ((-0.1) ^& (0.1)) 0.06 (0 ^& 0) 0.35 GradPad
linear = mkLinearGradient (mkStops [(black,0,1), (white,1,1)]) (0 ^& (-0.5)) (0 ^& 0.5) GradPad


example = circle 0.25 # fillTexture radial  # lw none <> square 1 # fillTexture linear # lw none
main = defaultMain $ example # scaleX 1 # pad 1.1