{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main = defaultMain (eqTriangle === square 1)