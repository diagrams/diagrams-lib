{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main = defaultMain (eqTriangle 1 === square 1 === octagon 1)