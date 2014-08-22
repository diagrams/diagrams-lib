import           Data.Maybe

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

clipPath :: Path R2
clipPath = square 2 # alignR

loopyStar :: Diagram B R2
loopyStar = mconcat
          . map (cubicSpline True)
          . pathVertices
          . star (StarSkip 3)
          $ regPoly 7 1

clippedStar :: Diagram B R2
clippedStar = clipTo clipPath (loopyStar # fc lightgray)

example :: Diagram B R2
example = position (zip pts dots)
       <> traceArrows # lc cyan
       <> clippedStar
       <> loopyStar

pts :: [P2]
pts = [ (-1) ^& 0.9, (-0.65) ^& 0.65, (-0.25) ^& 0.65, (-0.25) ^& 0.4
      , (-0.1) ^& 0.9, 0.1 ^& 0.9, 0.25 ^& 0.4, 0.25 ^& 0.65
      , 0.65 ^& 0.65, 1 ^& 0.9 ]

vecs :: [R2]
vecs = [unitX, unitY, unit_X, unit_Y]

tracePt :: P2 -> [Double]
tracePt p = map (maybe 0 magnitude) vs where
  vs = (rayTraceV p) <$> vecs <*> [clippedStar]

traceArrows :: Diagram B R2
traceArrows = mconcat $ map ptArrows pts where
  ptArrows p = mconcat $
               map (arrowAt' (with & headSize .~ 0.1) p)
               . catMaybes $ rayTraceV p <$> vecs <*> [clippedStar]

traces :: [[Double]]
traces = map tracePt pts

dots :: [Diagram B R2]
dots = repeat (circle 0.015 # fc red # lw 0)

main :: IO ()
main = do
  putStr $ unlines $ map show traces
  mainWith $ example # centerXY # pad 1.1
