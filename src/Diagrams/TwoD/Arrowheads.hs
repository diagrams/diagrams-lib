{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Arrowheads
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Standard arrowheads and tails. Each arrowhead or tail is designed
-- to be drawn filled, with a line width of 0, and is normalized to
-- fit inside a circle of diameter 1.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Arrowheads
       (
       -- * Arrowheads
       -- ** Standard arrowheads
         tri
       , dart
       , spike
       , thorn
       , missile
       , lineHead
       , noHead

       -- ** Configurable arrowheads
       -- | Creates arrowheads of the same shape as the standard heads but
       --   where the angle parameter is used to specify the angle to the top
       --   left point of the arrowhead.
       , arrowheadTriangle
       , arrowheadDart
       , arrowheadSpike
       , arrowheadThorn
       , arrowheadMissile

       -- * Arrow tails
       -- ** Standard arrow tails
       , tri'
       , dart'
       , spike'
       , thorn'
       , missile'
       , lineTail
       , noTail
       , quill
       , block

       -- ** Configurable arrow tails

       , arrowtailQuill
       , arrowtailBlock

       -- * Internals
       , ArrowHT
       ) where

import           Control.Lens            ((&), (.~))
import           Data.AffineSpace
import           Data.Default.Class
import           Data.Functor            ((<$>))
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             (mempty, (<>))
import           Data.VectorSpace

import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.CubicSpline    (cubicSpline)
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TwoD.Align
import           Diagrams.TwoD.Arc       (arc')
import           Diagrams.TwoD.Path      ()
import           Diagrams.TwoD.Polygons
import           Diagrams.TwoD.Shapes
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector    (e, unitX, unit_X)
import           Diagrams.Util           (( # ))

-----------------------------------------------------------------------------

type ArrowHT = Double -> Double -> (Path R2, Path R2)

htRadius :: Double
htRadius = 0.5

scaleR :: (Transformable t, Scalar (V t) ~ Double) => t -> t
scaleR = scale htRadius

unit_X2 :: R2
unit_X2 = scaleR unit_X

closedPath :: (Floating (Scalar v), Ord (Scalar v), InnerSpace v) => Trail v -> Path v
closedPath = pathFromTrail . closeTrail

-- Heads ------------------------------------------------------------------
--   > drawHead h = arrowAt' (with & arrowHead .~ h & shaftStyle %~ lw 0)
--   >         origin (r2 (0.001, 0))
--   >      <> square 0.5 # alignL # lw 0

-- | Isoceles triangle style. The above example specifies an angle of `2/5 Turn`.

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_tri25Ex.svg#diagram=tri25Ex&width=120>>

--   > tri25Ex = arrowAt' (with & arrowHead .~ arrowheadTriangle (2/5 \@\@ turn) & shaftStyle %~ lw 0)
--   >           origin (r2 (0.001, 0))
--   >        <> square 0.6 # alignL # lw 0
arrowheadTriangle :: Angle -> ArrowHT
arrowheadTriangle theta = aHead
  where
    aHead size _ = (p, mempty)
      where
        p = polygon (def & polyType .~ PolyPolar [theta, (negateV 2 *^ theta)]
            (repeat (htRadius * size)) & polyOrient .~ NoOrient)  # alignL

-- | Isoceles triangle with linear concave base. Inkscape type 1 - dart like.
arrowheadDart :: Angle -> ArrowHT
arrowheadDart theta = aHead
  where
    aHead size shaftWidth = (dartP # moveOriginTo (dartVertices !! 2), joint)
      where
        r = htRadius * size
        dartP = polygon
                ( def & polyType .~ PolyPolar [theta, (1/2 @@ turn) ^-^ theta, (1/2 @@ turn) ^-^ theta]
                                              [r, r, 0.1 * size, r]
                      & polyOrient .~ NoOrient
                )
        dartVertices =  (concat . pathVertices) $ dartP
        m = magnitude (dartVertices !! 1 .-. dartVertices !! 3)
        s = 1 - shaftWidth / m
        v1 = if s > 0 then (dartVertices !! 1 .-. dartVertices !! 2) # scale s else zeroV
        v2 = if s > 0 then (dartVertices !! 3 .-. dartVertices !! 2) # scale s else zeroV
        joint = (closedPath $ trailFromVertices [ dartVertices !! 2
                             , dartVertices !! 1 .-^ v1
                             , dartVertices !! 3 .-^ v2
                             , dartVertices !! 2 ]) # alignR

-- | Isoceles triangle with curved concave base. Inkscape type 2.
arrowheadSpike :: Angle -> ArrowHT
arrowheadSpike theta = aHead
  where
    aHead size shaftWidth = (barb # moveOriginBy (m *^ unit_X) , joint)
      where
        a  = e theta # scaleR
        a' = reflectY a
        l1 = trailFromSegments [straight (unit_X2 ^+^ a)]
        l2 = trailFromSegments [reverseSegment . straight $ (unit_X2 ^+^ a')]
        c  = reflectX $ arc' htRadius theta (negateV theta)
        barb = (closedPath $ (l1 <> c <> l2)) # scale size
        m = xWidth barb 
        sinb = (shaftWidth / 2) / (htRadius * size)
        b =  if sinb < 1 then asin sinb @@ rad else pi/2 @@ rad
        c' = arc' htRadius (negateV b) b # scale size
        joint = (closedPath $ (c')) # centerY # alignR
        xWidth p = pa + pb
          where
            pa = fromMaybe 0 (magnitude <$> traceV origin unitX p)
            pb = fromMaybe 0 (magnitude <$> traceV origin unit_X p)

-- | Curved sides, linear concave base. Illustrator CS5 #3
arrowheadThorn :: Angle -> Double -> ArrowHT
arrowheadThorn theta r = aHead
  where
    aHead size shaftWidth = (thornP # moveOriginTo (thornVertices !! 2), joint)
      where
        a  = e theta # scaleR
        c1 = curvedSide theta
        l1 = straight $ (reflectY a) ^-^ (unit_X2 # scale r)
        l2 = straight $ unit_X2 # scale r ^-^ a
        c2 = c1 # rotate (negateV theta)
        thornP = (closedPath $ trailFromSegments [c1, l1, l2, c2]) # scale size
        thornVertices =  (concat . pathVertices) $ thornP
        m = magnitude (thornVertices !! 1 .-. thornVertices !! 3)
        s = 1 - shaftWidth / m
        v1 = if s > 0 then (thornVertices !! 1 .-. thornVertices !! 2) # scale s else zeroV
        v2 = if s > 0 then (thornVertices !! 3 .-. thornVertices !! 2) # scale s else zeroV
        joint = (closedPath $ trailFromVertices [ thornVertices !! 2
                             , thornVertices !! 1 .-^ v1
                             , thornVertices !! 3 .-^ v2
                             , thornVertices !! 2 ]) # alignR

-- | Make a side for the thorn head.
curvedSide :: Angle -> Segment Closed R2
curvedSide theta = bezier3 ctrl1 ctrl2 end
  where
    v0    = scaleR unit_X
    v1    = e theta # scaleR
    ctrl1 = v0 # scaleR
    ctrl2 = v0 ^+^ (v1 # scaleR)
    end   = v0 ^+^ v1

-- | Transform an arrowhead/tail by fitting a cubic spline to it's vertices.
--   XXX Rear vertices of the arrowhead will extend outside of the unit circle.
--   XXX and the joint is a rectancgle as opposed to the correct shape.
smoothArrowhead :: ArrowHT -> ArrowHT
smoothArrowhead f = aHead
  where
    aHead size shaftWidth = (h, j)
      where
        (h', _) = f size shaftWidth
        h = smooth $ pathVertices h'
        -- XXX replace square joint with actual shape
        j = square shaftWidth # scaleX 0.25 alignR
        smooth [] = mempty
        smooth (x:xs) = cubicSpline True x <> smooth xs

arrowheadMissile :: Angle -> ArrowHT
arrowheadMissile theta = smoothArrowhead $ arrowheadDart theta

-- Standard heads ---------------------------------------------------------
-- | A line the same width as the shaft.
lineHead :: ArrowHT
lineHead s w = (square 1 # scaleX s # scaleY w # alignL, mempty)

noHead :: ArrowHT
noHead _ _ = (mempty, mempty)

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_triEx.svg#diagram=triEx&width=100>>

--   > triEx = drawHead tri
tri :: ArrowHT
tri = arrowheadTriangle (1/3 @@ turn)

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_spikeEx.svg#diagram=spikeEx&width=100>>

--   > spikeEx = drawHead spike
spike :: ArrowHT
spike = arrowheadSpike (3/8 @@ turn)

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_thornEx.svg#diagram=thornEx&width=100>>

--   > thornEx = drawHead thorn
thorn :: ArrowHT
thorn = arrowheadThorn (3/8 @@ turn) 1

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_dartEx.svg#diagram=dartEx&width=100>>

--   > dartEx = drawHead dart
dart :: ArrowHT
dart = arrowheadDart (2/5 @@ turn)

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_missileEx.svg#diagram=missileEx&width=100>>

--   > missileEx = drawHead missile
missile :: ArrowHT
missile = arrowheadMissile (2/5 @@ turn)

-- Tails ------------------------------------------------------------------
--   > drawTail t = arrowAt' (with  & arrowTail .~ t & shaftStyle %~ lw 0 & arrowHead .~ noHead)
--   >         origin (r2 (0.001, 0))
--   >      <> square 0.5 # alignL # lw 0

-- | Utility function to convert any arrowhead to an arrowtail, i.e.
--   attached at the start of the trail.
headToTail :: ArrowHT -> ArrowHT
headToTail hd = tl
  where
    tl size shaftWidth = (t, j)
      where
        (t', j') = hd size shaftWidth
        t = reflectX t'
        j = reflectX j'

arrowtailBlock :: Angle -> ArrowHT
arrowtailBlock theta = aTail
  where
   aTail size _ = (t, mempty)
      where
        t = square 1 # scaleX x # scaleY y # scale size # alignR
        a'  = e theta # scaleR
        a = a' ^-^ (reflectY a')
        y = magnitude a
        b = a' ^-^ (reflectX a')
        x = magnitude b

-- | The angle is where the top left corner intersects the circle.
arrowtailQuill :: Angle -> ArrowHT
arrowtailQuill theta =aTail
  where
   aTail size shaftWidth = (t, j)
      where
        t = ( closedPath $ trailFromVertices [v0, v1, v2, v3, v4, v5, v0] )
            # scale size # alignR
        v0 = p2 (0.5, 0)
        v2 = p2 (unr2 $ e theta # scaleR)
        v1 = v2 # translateX (5/4 * htRadius)
        v3 = p2 (-0.1, 0)
        v4 = v2 # reflectY
        v5 = v4 # translateX (5/4 * htRadius)
        s = 1 - shaftWidth / magnitude (v1 .-. v5)
        n1 = v0 # translateY (0.5 * shaftWidth)
        n2 = v1 .-^ ((v1 .-. v0) # scale s)
        n3 = v5 .-^ ((v5 .-. v0) # scale s)
        n4 = n1 # reflectY
        j = ( closedPath $ trailFromVertices
                [ v0, n1, n2, v0, n3, n4, v0 ])

-- Standard tails ---------------------------------------------------------
-- | A line the same width as the shaft.
lineTail :: ArrowHT
lineTail s w = (square 1 # scaleY w # scaleX s # alignR, mempty)

noTail :: ArrowHT
noTail _ _ = (mempty, mempty)

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_tri'Ex.svg#diagram=tri'Ex&width=100>>

--   > tri'Ex = drawTail tri'
tri' :: ArrowHT
tri' = headToTail tri

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_spike'Ex.svg#diagram=spike'Ex&width=100>>

--   > spike'Ex = drawTail spike'
spike' :: ArrowHT
spike' = headToTail spike

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_thorn'Ex.svg#diagram=thorn'Ex&width=100>>

--   > thorn'Ex = drawTail thorn'
thorn' :: ArrowHT
thorn' = headToTail thorn

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_dart'Ex.svg#diagram=dart'Ex&width=100>>

--   > dart'Ex = drawTail dart'
dart' :: ArrowHT
dart' = headToTail dart

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_missile'Ex.svg#diagram=missile'Ex&width=100>>

--   > missile'Ex = drawTail missile'
missile' :: ArrowHT
missile' = headToTail missile

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_quillEx.svg#diagram=quillEx&width=100>>

--   > quillEx = drawTail quill
quill :: ArrowHT
quill = arrowtailQuill (2/5 @@ turn)

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_blockEx.svg#diagram=blockEx&width=100>>

--   > blockEx = drawTail block
block :: ArrowHT
block = arrowtailBlock (2/5 @@ turn)
