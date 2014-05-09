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

       -- * Arrow tails
       -- ** Standard arrow tails
       , tri'
       , dart'
       , spike'
       , thorn'
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

import           Control.Lens            ((&), (.~), (^.))
import           Data.AffineSpace
import           Data.Default.Class
import           Data.Functor            ((<$>))
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             (mempty, (<>))
import           Data.VectorSpace

import           Diagrams.Angle
import           Diagrams.Core

import           Diagrams.Coordinates    ((^&))
import           Diagrams.CubicSpline    (cubicSpline)
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TrailLike      (fromOffsets)
import           Diagrams.TwoD.Align
import           Diagrams.TwoD.Arc       (arc')
import           Diagrams.TwoD.Path      ()
import           Diagrams.TwoD.Polygons
import           Diagrams.TwoD.Shapes
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector    (fromDirection, direction, e, unitX, unit_X)
import           Diagrams.Util           (( # ))

-----------------------------------------------------------------------------

type ArrowHT = Double -> Double -> (Path R2, Path R2)

htRadius :: Double
htRadius = 0.5

scaleR :: (Transformable t, Scalar (V t) ~ Double) => t -> t
scaleR = scale htRadius

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
    aHead len _ = (p, mempty)
      where
        psi = pi - (theta ^. rad)
        r = len / (1 + cos psi)
        p = polygon (def & polyType .~ PolyPolar [theta, (negateV 2 *^ theta)]
            (repeat r) & polyOrient .~ NoOrient)  # alignL


-- | Isoceles triangle with linear concave base. Inkscape type 1 - dart like.
arrowheadDart :: Angle -> ArrowHT
arrowheadDart theta len shaftWidth = (hd # scale size, jt)
  where
    hd = snugL . pathFromTrail . glueTrail $ fromOffsets [t1, t2, b2, b1]
    jt = pathFromTrail . glueTrail $ j <> reflectY j
    j = closeTrail $ fromOffsets [(-jLength ^& 0), (0 ^& shaftWidth / 2)]
    v = fromDirection theta
    (t1, t2) = (unit_X ^+^ v, (-0.5 ^& 0) ^-^ v)
    [b1, b2] = map (reflectY . negateV) [t1, t2]
    psi = pi - (direction . negateV $ t2) ^. rad
    jLength = shaftWidth / (2 * tan psi)

    -- If the shaft if too wide, set the size to a default value of 1.
    size = max 1 ((len - jLength) / (1.5))

-- | Isoceles triangle with curved concave base. Inkscape type 2.
arrowheadSpike :: ArrowHT
arrowheadSpike len shaftWidth  = (hd # scale r, jt # scale r)
  where
    hd = snugL . closedPath $ l1 <> c <> l2
    jt = alignR . centerY . pathFromTrail
                . closeTrail $ arc' 1 (negateV phi) phi
    l1 = trailFromSegments [straight $ unit_X ^+^ v]
    l2 = trailFromSegments [reverseSegment . straight $ (unit_X ^+^ (reflectY v))]
    c = reflectX $ arc' 1 (a @@ rad) (-a @@ rad)
    v = fromDirection (a @@ rad)
    a = 2 * pi / 3 
    x = shaftWidth / 2

    -- If the shaft is too wide for the head, we default the radius r to
    -- 2/3 * len by setting d=1 and phi=pi/2.
    d = max 1 (len^2 - 3 * x^2)
    r = 1/3 * (sqrt d + 2 * len)
    phi = asin (min 1 (x/r)) @@ rad

-- | Curved sides, linear concave base. Illustrator CS5 #3
arrowheadThorn :: Angle -> ArrowHT
arrowheadThorn theta len shaftWidth = (hd # scale size, jt)
  where
    hd = snugL . pathFromTrail . glueTrail $ hTop <> reflectY hTop
    hTop = closeTrail . trailFromSegments $ [c, l]
    jt = pathFromTrail . glueTrail $ j <> reflectY j
    j = closeTrail $ fromOffsets [(-jLength ^& 0), (0 ^& shaftWidth / 2)]
    c = curvedSide theta
    v = fromDirection theta
    l = reverseSegment . straight $ t
    t = v ^-^ (-0.5 ^& 0)
    psi = pi - (direction . negateV $ t) ^. rad
    jLength = shaftWidth / (2 * tan psi)

    -- If the shaft if too wide, set the size to a default value of 1.    
    size = max 1 ((len - jLength) / (1.5))

-- | Make a side for the thorn head.
curvedSide :: Angle -> Segment Closed R2
curvedSide theta = bezier3 ctrl1 ctrl2 end
  where
    v0    = unit_X
    v1    = fromDirection theta
    ctrl1 = v0
    ctrl2 = v0 ^+^ v1
    end   = v0 ^+^ v1

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
spike = arrowheadSpike

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_thornEx.svg#diagram=thornEx&width=100>>

--   > thornEx = drawHead thorn
thorn :: ArrowHT
thorn = arrowheadThorn (3/8 @@ turn) 

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_dartEx.svg#diagram=dartEx&width=100>>

--   > dartEx = drawHead dart
dart :: ArrowHT
dart = arrowheadDart (2/5 @@ turn)

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

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_quillEx.svg#diagram=quillEx&width=100>>

--   > quillEx = drawTail quill
quill :: ArrowHT
quill = arrowtailQuill (2/5 @@ turn)

-- | <<diagrams/src_Diagrams_TwoD_Arrowheads_blockEx.svg#diagram=blockEx&width=100>>

--   > blockEx = drawTail block
block :: ArrowHT
block = arrowtailBlock (2/5 @@ turn)
