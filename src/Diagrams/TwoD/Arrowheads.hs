{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Arrowheads
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Standard arrowheads and tails. Normalized to fit inside a circle of
-- diameter 1 when line width is set to 0. Designed to be filled.
-- XXX Most of these heads could be used unfilled if a small enough
-- XXX stroke width is used.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Arrowheads
       ( ArrowHT
       -- Heads --
       , arrowheadTriangle
       , arrowheadDart
       , arrowheadSpike
       , arrowheadThorn
       , arrowheadMissile

       , tri
       , dart
       , spike
       , thorn
       , missile
       , noHead

       -- Tails --
       , arrowtailQuill
       , arrowtailBlock

       , tri'
       , dart'
       , spike'
       , thorn'
       , missile'
       , noTail
       , quill
       , block
       ) where

import           Data.Maybe               (fromMaybe)
import           Data.Functor             ((<$>))
import           Data.Monoid              (mempty, (<>))
import           Data.VectorSpace
import           Data.AffineSpace
import           Diagrams.Core            hiding (radius)

import           Diagrams.Trail
import           Diagrams.Path
import           Diagrams.TwoD.Path       ()
import           Diagrams.Segment
import           Diagrams.TwoD.Shapes
import           Diagrams.TwoD.Polygons
import           Diagrams.TwoD.Arc        (arc')
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Align
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector     (unit_X, unitX, e)
import           Diagrams.Util            (with, ( # ))
import           Diagrams.CubicSpline     (cubicSpline)

-----------------------------------------------------------------------------

type ArrowHT = Double -> Double -> (Path R2, Path R2)

radius :: Double
radius = 0.5

scaleR :: (Transformable t, Scalar (V t) ~ Double) => t -> t
scaleR = scale radius

unit_X2 :: R2
unit_X2 = scaleR unit_X

closedPath :: (Floating (Scalar v), Ord (Scalar v), InnerSpace v) => Trail v -> Path v
closedPath = pathFromTrail . closeTrail

-- | Heads ------------------------------------------------------------------

-- | Isoceles triangle style.
arrowheadTriangle :: Angle a => a -> ArrowHT
arrowheadTriangle theta = aHead
  where
    aHead size _ = (p, mempty)
      where
        p = polygon with {polyType = PolyPolar [theta, (-2 * theta)]
            (repeat (radius * size)) ,polyOrient = NoOrient}  # alignL

-- | Isoceles triangle with linear concave base. Inkscape type 1 - dart like.
arrowheadDart :: Angle a => a -> ArrowHT
arrowheadDart theta = aHead
  where
    aHead size shaftWidth = (dartP # moveOriginTo (dartVertices !! 2), joint)
      where
        a = toTurn theta
        r = radius * size
        dartP = polygon with {polyType = PolyPolar [a, 1/2 - a, 1/2 - a]
               [r, r, 0.1 * size, r] ,polyOrient = NoOrient}
        dartVertices =  (concat . pathVertices) $ dartP
        m = magnitude (dartVertices !! 1 .-. dartVertices !! 3)
        s = 1 - shaftWidth / m
        v1 = (dartVertices !! 1 .-. dartVertices !! 2) # scale s
        v2 = (dartVertices !! 3 .-. dartVertices !! 2) # scale s
        joint = (closedPath $ trailFromVertices [ dartVertices !! 2
                             , dartVertices !! 1 .-^ v1
                             , dartVertices !! 3 .-^ v2
                             , dartVertices !! 2 ]) # alignR

-- | Isoceles triangle with curved concave base. Inkscape type 2.
arrowheadSpike :: Angle a => a -> ArrowHT
arrowheadSpike theta = aHead
  where
    aHead size shaftWidth = (barb # moveOriginBy (m *^ unit_X) , joint)
      where
        a  = e theta # scaleR
        a' = reflectY a
        l1 = trailFromSegments [straight (unit_X2 ^+^ a)]
        l2 = trailFromSegments [reverseSegment . straight $ (unit_X2 ^+^ a')]
        c  = reflectX $ arc' radius theta (-theta)
        barb = (closedPath $ (l1 <> c <> l2)) # scale size
        m = xWidth barb --c `atParam` 0.5
        b =  Rad $ asin ((shaftWidth / 2) / (radius  * size))
        c' = arc' radius (-b ) b # scale size
        joint = (closedPath $ (c')) # centerY # alignR
        xWidth p = pa + pb
          where
            pa = fromMaybe 0 (magnitude <$> traceV origin unitX p)
            pb = fromMaybe 0 (magnitude <$> traceV origin unit_X p)

-- | Curved sides, linear concave base. Illustrator CS5 #3
arrowheadThorn :: Angle a => a -> Double -> ArrowHT
arrowheadThorn theta r = aHead
  where
    aHead size shaftWidth = (thornP # moveOriginTo (thornVertices !! 2), joint)
      where
        a  = e theta # scaleR
        c1 = curvedSide theta
        l1 = straight $ (reflectY a) ^-^ (unit_X2 # scale r)
        l2 = straight $ unit_X2 # scale r ^-^ a
        c2 = c1 # rotate (-theta)
        thornP = (closedPath $ trailFromSegments [c1, l1, l2, c2]) # scale size
        thornVertices =  (concat . pathVertices) $ thornP
        m = magnitude (thornVertices !! 1 .-. thornVertices !! 3)
        s = 1 - shaftWidth / m
        v1 = (thornVertices !! 1 .-. thornVertices !! 2) # scale s
        v2 = (thornVertices !! 3 .-. thornVertices !! 2) # scale s
        joint = (closedPath $ trailFromVertices [ thornVertices !! 2
                             , thornVertices !! 1 .-^ v1
                             , thornVertices !! 3 .-^ v2
                             , thornVertices !! 2 ]) # alignR

-- | Make a side for the thorn head.
curvedSide :: Angle a => a -> Segment Closed R2
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

arrowheadMissile :: Angle a => a -> ArrowHT
arrowheadMissile theta = smoothArrowhead $ arrowheadDart theta

-- | Standard heads ---------------------------------------------------------
noHead :: ArrowHT
noHead _ _ = (mempty, mempty)

tri :: ArrowHT
tri = arrowheadTriangle (1/3 :: Turn)

spike :: ArrowHT
spike = arrowheadSpike (3/8 :: Turn)

thorn :: ArrowHT
thorn = arrowheadThorn (3/8 :: Turn) 1

dart :: ArrowHT
dart = arrowheadDart (2/5 :: Turn)

missile :: ArrowHT
missile = arrowheadMissile (2/5 :: Turn)

-- | Tails ------------------------------------------------------------------

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

arrowtailBlock :: Angle a => a -> ArrowHT
arrowtailBlock theta =aTail
  where
   aTail size _ = (t, mempty)
      where
        t = square 1 # scaleX x # scaleY y # scale size # alignR
        a'  = e theta # scaleR
        a = a' ^-^ (reflectY a')
        y = magnitude a
        b = a' ^-^ (reflectX a')
        x = magnitude b

-- | Theta is the angle where the top left corner intersects the circle.
arrowtailQuill :: Angle a => a -> ArrowHT
arrowtailQuill theta =aTail
  where
   aTail size shaftWidth = (t, j)
      where
        t = ( closedPath $ trailFromVertices [v0, v1, v2, v3, v4, v5, v0] )
            # scale size # alignR
        theta' = toTurn theta
        v0 = p2 (0.5, 0)
        v2 = p2 (unr2 $ e theta' # scaleR)
        v1 = v2 # translateX (5/4 * radius)
        v3 = p2 (-0.1, 0)
        v4 = v2 # reflectY
        v5 = v4 # translateX (5/4 * radius)
        s = 1 - shaftWidth / magnitude (v1 .-. v5)
        n1 = v0 # translateY (0.5 * shaftWidth)
        n2 = v1 .-^ ((v1 .-. v0) # scale s)
        n3 = v5 .-^ ((v5 .-. v0) # scale s)
        n4 = n1 # reflectY
        j = ( closedPath $ trailFromVertices
                [ v0, n1, n2, v0, n3, n4, v0 ])

-- | Standard tails ---------------------------------------------------------
noTail :: ArrowHT
noTail _ _ = (mempty, mempty)

tri' :: ArrowHT
tri' = headToTail tri

spike' :: ArrowHT
spike' = headToTail spike

thorn' :: ArrowHT
thorn' = headToTail thorn

dart' :: ArrowHT
dart' = headToTail dart

missile' :: ArrowHT
missile' = headToTail missile

quill :: ArrowHT
quill = arrowtailQuill (2/5 :: Turn)

block :: ArrowHT
block = arrowtailBlock (2/5 :: Turn)
