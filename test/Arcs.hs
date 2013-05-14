import Diagrams.Prelude
import Diagrams.Backend.Postscript
import Diagrams.Backend.Postscript.CmdLine

import Diagrams.TwoD.Arc

exampleArc f r = (vertLabel |||) . centerXY . (horzLabel ===) . centerXY $ hcat 
    [ vcat 
      [  phantom (circle (1.05 * abs r) :: D R2) 
      <> s # lc green # lw 0.01
      <> e # lc red   # lw 0.01
      <> (lw 0.01 . stroke $ f r (n/8) (m/8))
      | n <- rs
      , let s = rotateBy (n/8) (origin ~~ (3 & 0))
      , let e = rotateBy (m/8) (origin ~~ (3 & 0))
      ] 
    | m <- rs
    ]
  where
    rs = [0..7 :: CircleFrac]
    horzLabel = centerX                  $ rect 5 10 # lw 0 <> (text "start angle" # scale 0.4)
    vertLabel = centerY . rotateBy (1/4) $ rect 5 10 # lw 0 <> (text "end angle"   # scale 0.4)
  
exampleRR :: Diagram Postscript R2
exampleRR = (vertLabel |||) . centerXY . (horzLabel ===) . centerXY $ hcat 
    [ vcat 
      [  phantom (pad 1.1 $ rect 10 15 :: D R2) 
      <> (origin ~~ (0 & r)) # lc red   # lw 0.01
      <> (fc lightblue . lw 0.01 . stroke $ roundedRect' 10 15 o)
      | o <- [ RoundedRectOpts 0 r 0 0
             , RoundedRectOpts r 0 0 0
             , RoundedRectOpts 0 0 r 0
             , RoundedRectOpts 0 0 0 r
             ]
      ] 
    | r <- [-4..4]
    ]
  where
    horzLabel = centerX                  $ rect 5 10 # lw 0 <> (text "radius [-4..4]" # scale 0.4)
    vertLabel = centerY . rotateBy (1/4) $ rect 5 10 # lw 0 <> (text "corner"         # scale 0.4)

arcs = [ ("arc'  CCW", exampleArc arc' 3)
       , ("arc'  CW" , exampleArc arc' (-3))
       , ("arc   CCW", exampleArc (\r s e -> arc s e # scale r) 3)
       , ("arcCW CCW", exampleArc (\r s e -> arcCW s e # scale (abs r)) (-3))
       ] :: [(String, Diagram Postscript R2)]

main = defaultMain (vcat (map snd arcs) === exampleRR)