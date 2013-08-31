{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Arrow
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Drawing arrows in two dimensions.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Arrow
       ( straightArrow
       , straightArrow'
       , arrow
       , arrow'
       , connect
       , connect'
       , connectTrail
       , connectTrail'
       , ArrowOpts(..)
       , module Diagrams.TwoD.Arrowheads
       ) where

import           Data.Functor             ((<$>))
import           Data.Default.Class
import           Data.Monoid              (mempty, (<>))
import           Data.VectorSpace
import           Data.AffineSpace
import           Data.Maybe               (fromMaybe)
import           Diagrams.Core

import           Data.Colour       hiding (atop)
import           Data.Colour.Names        (black, blue, orange)
import           Diagrams.Trail
import           Diagrams.TrailLike
import           Diagrams.Attributes
import           Diagrams.Segment
import           Diagrams.Path
import           Diagrams.TwoD.Path       (strokeT)
import           Diagrams.TwoD.Shapes     (triangle)
import           Diagrams.TwoD.Transform  (rotateBy, rotate, scaleInvPrim, reflectX)
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector     (unitX, unit_X, direction)
import           Diagrams.TwoD.Size       (width)
import           Diagrams.TwoD.Arrowheads
import           Diagrams.Parametric
import           Diagrams.Util            (with, (#))

data ArrowOpts
  = ArrowOpts
    { arrowHead  :: ArrowHT  -- XXX investigate whether we can make
                             -- these Diagrams. Because of ScaleInv,
                             -- would have to have a Renderable
                             -- instance for Diagrams themselves.
                             -- This might be possible but there is
                             -- some trickiness involved.

                             -- However, just having paths does
                             -- simplify a lot of things (no type
                             -- parameter required for ArrowOpts, no
                             -- ScopedTypeVariables, etc.
    , arrowTail    :: ArrowHT
    , headSize     :: Double
    , tailSize     :: Double
    , headGap      :: Double
    , tailGap      :: Double
    , shaftWidth   :: Double
    , headStyle    :: HasStyle c => c -> c
    , tailStyle    :: HasStyle c => c -> c
    , shaftStyle   :: HasStyle c => c -> c
    }

instance Default ArrowOpts where
  def = ArrowOpts
        { arrowHead    = dart
        , arrowTail    = noTail
        , headSize     = 0.3
        , tailSize     = 0.3
        , headGap      = 0 -- amount of space to leave after arrowhead
        , tailGap      = 0 -- amount of space ot leave before arrowtail
        , shaftWidth   = 0.03
        , headStyle    = fc blue . opacity 0.7   -- XXX change to black, 1
        , tailStyle    = fc orange . opacity 0.7 -- XXX change to black, 1
        , shaftStyle   = fc black . opacity 1
        }

-- | Invert a monotonic not decreasing function.
--   Search for x between low and high for the input to the
--   function f such that: |f(x) - target| < epsilon. If the search
--   has not converged after iter iterations then return the current guess.
--   If target is less than f(low) return low, if target is greater than
--   f(high) then return high.
fInverse :: (Double -> Double)
           -> Double -> Int -> Double -> Double -> Double -> Double
fInverse f epsilon iter low high target
  | iter < 1 = mid
  | f low > target = low
  | f high < target = high
  | f mid > target + epsilon = fInverse f epsilon (iter - 1) low mid target
  | f mid < target - epsilon = fInverse f epsilon (iter - 1) mid high target
  | otherwise = mid
  where mid = (low + high) / 2

-- | Default number of iterations to try in hParam and tParam before giving up.
stdIterations :: Int
stdIterations = 64

-- | Return the parameter p of the point on tr such that the distance from
--   tr `atParam` p to the end of tr is equal to w.
hParam :: Trail R2 -> Double -> Double
hParam tr w = (domainUpper tr) - fInverse (\x -> magnitude $ hOffset x)
              stdTolerance stdIterations (domainLower tr) (domainUpper tr) w
  where
    hOffset x = (reverseDomain tr) `atParam` x

-- | Same as above but for the start of tr.
tParam :: Trail R2 -> Double -> Double
tParam tr w = fInverse (\x -> magnitude $ tOffset x)
              stdTolerance stdIterations (domainLower tr) (domainUpper tr) w
  where
    tOffset x = tr `atParam` x

-- | Calculate the length of the portion of the horizontal line that passes
--   through the origin and is inside of p.
xWidth :: (Traced t, V t ~ R2) => t -> Double
xWidth p = a + b
  where
    a = fromMaybe 0 (magnitude <$> traceV origin unitX p)
    b = fromMaybe 0 (magnitude <$> traceV origin unit_X p)

-- | Get the line color from the shaft to use as the fill color for the joint.
colorJoint :: (Style v -> Style v) -> Style v
colorJoint sStyle =
    let c = fmap getLineColor . getAttr $ sStyle mempty in
    case c of
        Nothing -> mempty
        Just c' -> fillColor c' $ mempty

-- | Combine the head and it's joint into a single scale invariant diagram
--   and move the origin to the attachment point. Return the diagram it width.
mkHead :: Renderable (Path R2) b => ArrowOpts -> (Diagram b R2, Double)
mkHead opts = ( (j <> h) # moveOriginBy (jWidth *^ unit_X) # lw 0
              , hWidth + jWidth)
  where
    (h', j') = (arrowHead opts) (headSize opts) (shaftWidth opts)
    hWidth = xWidth h'
    jWidth = xWidth j'
    h = scaleInvPrim h' unitX # (headStyle opts)
    j = scaleInvPrim j' unitX # (shaftStyle opts)
                              # applyStyle (colorJoint (shaftStyle opts))

-- | Just like mkHead only the attachment point is on the right.
mkTail :: Renderable (Path R2) b => ArrowOpts -> (Diagram b R2, Double)
mkTail opts = ( (t <> j) # moveOriginBy (jWidth *^ unitX) # lw 0
              , tWidth + jWidth)
  where
    (t', j') = (arrowTail opts) (tailSize opts) (shaftWidth opts)
    tWidth = xWidth t'
    jWidth = xWidth j'
    t = scaleInvPrim t' unitX # (tailStyle opts)
    j = scaleInvPrim j' unitX # (shaftStyle opts)
                              # applyStyle (colorJoint (shaftStyle opts))

-- | Find the vector pointing in the direction of the segment at it's endpoint.
endTangent :: Segment Closed R2 -> R2
endTangent (Cubic c1 c2 (OffsetClosed x2)) = (normalized (x2 ^-^ c2))
endTangent (Linear (OffsetClosed x1)) = normalized x1

-- | Find the vector pointing in the direction of the segment away from
--   it's starting point.
startTangent :: Segment Closed R2 -> R2
startTangent (Cubic c1 c2 (OffsetClosed x2)) = (normalized c1)
startTangent (Linear (OffsetClosed x1)) = (normalized x1)

-- | Make a trail with the same angles and offset as an arrow with tail width
--   tw, head width hw and shaft of tr, such that the magnituted of the shaft
--   offset is size. Used for calculating the offset of an arrow.
spine :: Trail R2 -> Double -> Double -> Double -> Trail R2
spine tr tw hw size = tS <> shaft <> hS
  where
    tAngle = direction . startTangent $ (head $ trailSegments tr) :: Turn
    hAngle = direction . endTangent $ (last $ trailSegments tr) :: Turn
    shaft = tr # scale size
    hSpine = trailFromOffsets [unitX] # scale hw # rotateBy hAngle
    tSpine = trailFromOffsets [unitX] # scale tw # rotateBy tAngle
    hS = if hw > 0 then hSpine else mempty
    tS = if tw > 0 then tSpine else mempty

-- | Calculate the amount required to scale a shaft trail so that an arrow with
--   head width hw and tail width tw has offset t. We use spine intead of the
--   arrow itself, since spine is a trail with the same offset and a trail
--   offset is readily available.
shaftScale :: Trail R2 -> Double -> Double -> Double -> Double
shaftScale tr tw hw t =
    fInverse f stdTolerance stdIterations (1 / stdScale) stdScale t
  where
    f = magnitude . trailOffset . (spine tr tw hw)

-- | Maximum scaling adjustment that can be applied to a trail to make the arrow
--   reach from it's starting point to it' end point.
stdScale :: Double
stdScale = 100

-- | Make an arrow pointing from s to e using the trail tr by scaling the trail
--   and rotating the arrow. The size of trail tr is arbitrary since it will be
--   scaled so that the arrow offset is e .-. s, however is should be not be
--   off by more than a factor of 100!
arrow :: Renderable (Path R2) b =>Trail R2 -> P2 -> P2 -> Diagram b R2
arrow tr s e = arrow' def tr s e

arrow'
  :: Renderable (Path R2) b =>
     ArrowOpts -> Trail R2 -> P2 -> P2 -> Diagram b R2
arrow' opts tr s e = a # rotateBy (a0 - a1) # moveTo s
  where
    (h, hw') = mkHead opts
    (t, tw') = mkTail opts
    tw = tw' + tailGap opts
    hw = hw' + headGap opts
    tAngle = direction . startTangent $ (head $ trailSegments tr) :: Turn
    hAngle = direction . endTangent $ (last $ trailSegments tr) :: Turn
    u = e .-. s
    sd = shaftScale tr tw hw (magnitude u)
    tr' = tr # scale sd
    shaft = strokeT tr'
        # lw (shaftWidth opts)
        # (shaftStyle opts)
    hd = h # rotateBy hAngle # moveTo (origin .+^ tr' `atParam` (domainUpper tr'))
    tl = t # rotateBy tAngle
    a0 = direction u
    a1 = direction (trailOffset $ spine tr tw hw sd)
    a = moveOriginBy ((tw *^ (unit_X # rotateBy tAngle))) $ hd <> tl <> shaft

-- | Make an arrow from s to e with a straight line shaft.
straightArrow :: Renderable (Path R2) b => P2 -> P2 -> Diagram b R2
straightArrow = straightArrow' def

straightArrow' :: Renderable (Path R2) b => ArrowOpts -> P2 -> P2 -> Diagram b R2
straightArrow' opts s e = arrow' opts tr s e
  where tr = trailFromSegments [straight (e .-. s)]

-- | Connect two diagrams with a straight arrow.
connect
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
  => n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connect = connect' def

connect'
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
  => ArrowOpts -> n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connect' opts n1 n2 =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [s,e] = map location [sub1, sub2]
    in  atop (straightArrow' opts s e)

-- | Connect two diagrams with an arbitrary trail, trail scale is irrelevant
connectTrail
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
  => Trail R2 -> n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connectTrail tr n1 n2 = connectTrail' def tr n1 n2

connectTrail'
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
  => ArrowOpts -> Trail R2 -> n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connectTrail' opts tr n1 n2 =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [s,e] = map location [sub1, sub2]
    in  atop (arrow' opts tr s e)

-- | Connect two diagrams with an aribrary trail at point on the perimeter of
-- | the diagrams, choosen by angle.
connectPerim'
  :: (Renderable (Path R2) b, IsName n1, IsName n2, Angle a)
  => ArrowOpts -> Trail R2 -> n1 -> n2 -> a -> a
  -> (Diagram b R2 -> Diagram b R2)
connectPerim' opts tr n1 n2 a1 a2 =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [os, oe] = map location [sub1, sub2]
        s = fromMaybe os (maxTraceP os (unitX # rotate a1) sub1)
        e = fromMaybe oe (maxTraceP oe (unitX # rotate a2) sub2)
    in  atop (arrow' opts tr s e)