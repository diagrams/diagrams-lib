{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.List.Split              (chunksOf)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

-- Create a 3 x 3 grid of circles named "1" to "9"
c = circle 1.5 # fc lightgray # lw none # showOrigin
cs = [c # named (show x) | x <- [1..9]]
cGrid = (vcat' $ with & sep .~ 4)
      . map (hcat' $ with & sep .~ 12)
      . chunksOf 3 $ cs

-- For the Shafts.
semicircle = arc (5/12 @@ turn) (11/12 @@ turn)
quartercircle = arc (1/2 @@ turn) (3/4 @@ turn)

parab = bezier3 (1 ^& 1) (1 ^& 1) (0 ^& 2)
parab' = reflectX parab

seg = straight unitX
seg' = seg # rotateBy (1/6)

shaft0 = trailFromSegments [parab, seg, parab', seg, parab]
shaft1 = cubicSpline False (trailVertices (shaft0 `at` origin))
shaft2 = cubicSpline False (map p2 [(0,0), (1,0), (0.8, 0.2),(2, 0.2)])

example :: Diagram B R2
example = connect'        arrow1 "1" "2"
        . connect'        arrow2 "4" "3"
        . connect'        arrow3 "1" "6"
        . connectOutside' arrow4 "4" "8"
        . connect'        arrow5 "9" "5"
        . connectOutside' arrow6 "8" "9"
        . connectOutside' arrow7 "8" "7"
        $ cGrid

  where
    -- The arrows
    arrow1 = with & arrowHead .~ dart
                  & arrowTail .~ quill & shaftStyle %~ lw thick . lc black
                  & arrowShaft .~ shaft0 & headStyle %~ fc blue
                  & tailStyle %~ fc red

    arrow2 = with & arrowHead .~ dart
                  & arrowTail .~ dart'
                  & shaftStyle %~ lw thin & arrowShaft .~ shaft1

    arrow3 = with & arrowHead .~ thorn & headLength .~ large
                  & arrowShaft .~ quartercircle & arrowTail .~ noTail
                  & gaps .~ normal

    arrow4 = with & arrowHead .~ dart & arrowTail .~ dart'
                  & arrowShaft .~ shaft2 & headStyle %~ fc teal
                  & tailStyle %~ fc teal & shaftStyle %~ lw thick . lc teal

    arrow5 = with & arrowTail .~ spike' & tailLength .~ large
                  & arrowShaft .~ semicircle & arrowHead .~ spike
                  & headLength .~ large & headStyle %~ fc darkorange
                  & tailStyle %~ fc darkorange
                  & shaftStyle %~ lw veryThick . lc navy

    arrow6 = with & arrowHead .~ tri & arrowTail .~ tri'
                  & headLength .~ large
                  & headStyle %~ fc black . opacity 0.5
                  & tailStyle %~ fc black . opacity 0.5
                  & shaftStyle %~ dashingN [0.01,0.02,0.03,0.01] 0

    arrow7 = arrow6 & arrowHead .~ tri & arrowTail .~ tri'

main = mainWith $ example # frame 0.25
