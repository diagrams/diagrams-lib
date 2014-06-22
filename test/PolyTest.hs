{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Cairo.CmdLine
import           Diagrams.Prelude

import           Diagrams.TwoD.Polygons

-- d = stroke . close $ fromVertices (polyPoints with { polyStar = StarFun succ })

vs = take 10 $ iterate (rotateBy (1/20 :: CircleFrac)) unitX

mkR v = (mconcat . mconcat $ p)
     <> fromVertices [origin, origin .+^ v]
  where
    p = map (zipWith lc (red : repeat black)) $
        (map (map stroke))
        (explodePath (polygon (with & polyOrient .~ OrientTo v )))

d = hcat' with {sep = 0.5} (map mkR vs)
  # lw 0.05

s = stroke $ starPoly (StarSkip 5)
               (polygon (with & polyType .~ PolyPolar
                                            (repeat (tau/15 :: Rad))
                                            (take 15 (cycle [6,7,8]))
                             ))

main = defaultMain (pad 1.1 s)
