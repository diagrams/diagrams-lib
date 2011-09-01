{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.TwoD.Polygons

d = stroke . close $ fromVertices (polySides [3/8, 3/8, 3/8, 3/8 :: CircleFrac]
                                             [1,2,0.5,6,1]
                                  )

main = defaultMain (pad 1.1 (d <> hrule 6 <> vrule 6))