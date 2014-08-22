{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

main = defaultMain (eqTriangle 1 === square 1 === octagon 1)
