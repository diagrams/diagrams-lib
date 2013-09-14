{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
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
       ( arrow
       , arrow'
       , arrowAt
       , arrowAt'
       , arrowBetween
       , arrowBetween'
       , connect
       , connect'
       , connectPerim
       , connectPerim'
       , straightShaft
       , ArrowOpts(..)
       , module Diagrams.TwoD.Arrowheads
       ) where

import           Data.AffineSpace
import           Data.Default.Class
import           Data.Functor                     ((<$>))
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      (mempty, (<>))
import           Data.VectorSpace
import           Diagrams.Core

import           Data.Colour                      hiding (atop)
import           Diagrams.Attributes
import           Diagrams.Parametric
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TwoD.Arrowheads
import           Diagrams.TwoD.Path               (strokeT)
import           Diagrams.TwoD.Transform          (rotate, rotateBy)
import           Diagrams.TwoD.Transform.ScaleInv (scaleInvPrim)
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector             (direction, unitX, unit_X)
import           Diagrams.Util                    (( # ))

data ArrowOpts
  = ArrowOpts
    { arrowHead  :: ArrowHT
    , arrowTail  :: ArrowHT
    , arrowShaft :: Trail R2
    , headSize   :: Double
    , tailSize   :: Double
    , headGap    :: Double -- amount of space to leave after arrowhead
    , tailGap    :: Double -- amount of space ot leave before arrowtail
    , shaftWidth :: Double
    , headStyle  :: HasStyle c => c -> c
    , tailStyle  :: HasStyle c => c -> c
    , shaftStyle :: HasStyle c => c -> c
    }

straightShaft :: Trail R2
straightShaft = trailFromOffsets [unitX]

instance Default ArrowOpts where
  def = ArrowOpts
        { arrowHead    = dart
        , arrowTail    = noTail
        , arrowShaft   = trailFromOffsets [unitX]
        , headSize     = 0.3
        , tailSize     = 0.3
        , headGap      = 0
        , tailGap      = 0
        , shaftWidth   = 0.03

        -- See note [Default arrow style attributes]
        , headStyle    = fc black
        , tailStyle    = fc black
        , shaftStyle   = lc black
        }

{- ~~~~ Note [Default arrow style attributes]

We want to set as few default attributes as possible.  The reason is
that any attributes which get set by default cannot be overridden
later.  For example, if we set 'opacity 1' in shaftStyle, then it
means arrow shafts are always fully opaque (unless you set the opacity
in shaftStyle).  That is, e.g.

  arrowBetween p q # opacity 0.5

will produce exactly the same output as arrowBetween p q, because the
opacity is already set in the arrowBetween call and cannot be
overridden later.

So why set the fill color and line color to black?

  1. The default shaftStyle *must* be set to lc black, because we get
     the joint fill color from the line color set in shaftStyle.  There is
     no way to tell when the user has applied a line color externally to
     the arrow function and magically have the joint fill color update to
     match.  The downside is that the *only* way to change the color of an
     arrow shaft is to modify shaftStyle.  Well, tough luck.

  2. Setting the head and tailStyle to 'id' does not work, because
     then they have the diagrams-default fill, i.e. transparent.

  3. We could set the head and tailStyle using 'recommendFillColor'
     instead of 'fc'.  That way they would be filled black by default
     but could be overridden externally by the user, e.g.

       arrowBetween p q # fc orange

     would change the arrowhead to orange.  However, this just seems
     inconsistent with the fact that # lc orange has no effect (see #1
     above).  So instead we simply set the default head/tail fill with
     'fc' and force the user to override it in the ArrowOpts, just as
     with line color for the shaft.
-}

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

-- XXX hParam and tParam are not used in the current implementation, but may
-- XXX be useful, if we add a function that shortens the arrow shaft by the
-- XXX head width, instead of scaling it, as is currently done.

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
-- XXX It may be possible to do away with this function by utilizing the new
-- XXX snug functions in TwoD.Align.
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
--   and move the origin to the attachment point. Return the diagram
--   and it's width.
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
endTangent (Cubic _ c2 (OffsetClosed x2)) = (normalized (x2 ^-^ c2))
endTangent (Linear (OffsetClosed x1)) = normalized x1

-- | Find the vector pointing in the direction of the segment away from
--   it's starting point.
startTangent :: Segment Closed R2 -> R2
startTangent (Cubic c1 _ (OffsetClosed _)) = (normalized c1)
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

-- | Return an arrow of length len using arrowShaft by scaling the shaft
--   so that the entire arrow is length len. The size of arrowShaft is arbitrary
--   since it will be scaled however it should be not be off by more than a
--   factor of stdScale. The arrow offset is the direction unitX.
arrow :: Renderable (Path R2) b => Double -> Diagram b R2
arrow len = arrow' def len

arrow' :: Renderable (Path R2) b => ArrowOpts -> Double -> Diagram b R2
arrow' opts len = ar # rotateBy (- dir)
  where
    (h, hw') = mkHead opts
    (t, tw') = mkTail opts
    tr = arrowShaft opts
    tw = tw' + tailGap opts
    hw = hw' + headGap opts
    tAngle = direction . startTangent $ (head $ trailSegments tr) :: Turn
    hAngle = direction . endTangent $ (last $ trailSegments tr) :: Turn
    sd = shaftScale tr tw hw len
    tr' = tr # scale sd
    shaft = strokeT tr' # lw (shaftWidth opts) # (shaftStyle opts)
    hd = h # rotateBy hAngle # moveTo (origin .+^ tr' `atParam` (domainUpper tr'))
    tl = t # rotateBy tAngle
    dir = direction (trailOffset $ spine tr tw hw sd)
    ar = moveOriginBy ((tw *^ (unit_X # rotateBy tAngle))) $ hd <> tl <> shaft

-- | Make an arrow pointing from s to e using arrowShaft by scaling the shaft
--   and rotating the arrow. The size of arrowShaft is arbitrary since it will
--   be scaled so that the arrow offset is e .-. s, however it should be not be
--   off by more than a factor of stdScale.
arrowBetween :: Renderable (Path R2) b => P2 -> P2 -> Diagram b R2
arrowBetween s e = arrowBetween' def s e

arrowBetween'
  :: Renderable (Path R2) b =>
     ArrowOpts -> P2 -> P2 -> Diagram b R2
arrowBetween' opts s e = arrow' opts len # rotateBy dir # moveTo s
  where
    v = e .-. s
    len = magnitude v
    dir = direction v

-- | Create an arrow starting at s with length and direction determined by
--   the vectore v.
arrowAt :: Renderable (Path R2) b => P2 -> R2 -> Diagram b R2
arrowAt s v = arrowAt' def s v

arrowAt'
  :: Renderable (Path R2) b =>
     ArrowOpts -> P2 -> R2 -> Diagram b R2
arrowAt' opts s v = arrow' opts len # rotateBy dir # moveTo s
  where
    len = magnitude v
    dir = direction v

-- | Connect two diagrams with a straight arrow.
connect
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
  => n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connect = connect' def

-- | Connect two diagrams with an arbitrary arrow.
connect'
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
  => ArrowOpts -> n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connect' opts n1 n2 =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [s,e] = map location [sub1, sub2]
    in  atop (arrowBetween' opts s e)

-- | Connect two diagrams at point on the perimeter of he diagrams, choosen
--   by angle.
connectPerim
  :: (Renderable (Path R2) b, IsName n1, IsName n2, Angle a)
  => n1 -> n2 -> a -> a
  -> (Diagram b R2 -> Diagram b R2)
connectPerim = connectPerim' def

connectPerim'
  :: (Renderable (Path R2) b, IsName n1, IsName n2, Angle a)
  => ArrowOpts -> n1 -> n2 -> a -> a
  -> (Diagram b R2 -> Diagram b R2)
connectPerim' opts n1 n2 a1 a2 =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [os, oe] = map location [sub1, sub2]
        s = fromMaybe os (maxTraceP os (unitX # rotate a1) sub1)
        e = fromMaybe oe (maxTraceP oe (unitX # rotate a2) sub2)
    in  atop (arrowBetween' opts s e)
