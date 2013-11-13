{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Arrow
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Drawing arrows in two dimensions.  For a tutorial on drawing arrows
-- using this module, see the diagrams website:
-- <http://projects.haskell.org/diagrams/doc/arrow.html>.
--
-----------------------------------------------------------------------------


module Diagrams.TwoD.Arrow
       ( -- * Examples:
         -- ** Example 1
-- | <<diagrams/src_Diagrams_TwoD_Arrow_example1.svg#diagram=example1&width=500>>
--
--   > -- Connecting two diagrams at their origins.
--   >
--   > sq = square 2 # showOrigin # lc darkgray # lw 0.07
--   > ds = (sq # named "left") ||| strutX 3 ||| (sq # named "right")
--   >
--   > shaft  = cubicSpline False ( map r2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])
--   >
--   > example1 = ds # connect' (with & arrowHead .~ dart & headSize .~ 0.6
--   >                                & tailSize .~ 0.5 & arrowTail .~ quill
--   >                                & shaftStyle %~ lw 0.02 & arrowShaft .~ shaft)
--   >                                "left" "right" # pad 1.1

         -- ** Example 2

-- | <<diagrams/src_Diagrams_TwoD_Arrow_example2.svg#diagram=example2&width=500>>
--
--   > -- Comparing connect, connectPerim, and arrowAt.
--   >
--   > oct  = octagon 1 # lc darkgray # lw 0.10 # showOrigin
--   > dias = oct # named "first" ||| strut 3 ||| oct # named "second"
--   >
--   > -- Connect two diagrams and two points on their trails.
--   > ex12 = dias # connect "first" "second"
--   >             # connectPerim "first" "second" (15/16 :: Turn) (9/16 :: Turn)
--   >
--   > -- Place an arrow at (0,0) the size and direction of (0,1).
--   > ex3 = arrowAt origin unit_Y
--   >
--   > example2 = (ex12 <> ex3) # centerXY # pad 1.1

         -- * Documentation
         arrow
       , arrow'
       , arrowAt
       , arrowAt'
       , arrowBetween
       , arrowBetween'
       , connect
       , connect'
       , connectPerim
       , connectPerim'
       , connectOutside
       , connectOutside'
       , straightShaft
       , ArrowOpts(..)

       , arrowHead
       , arrowTail
       , arrowShaft
       , headSize
       , tailSize
       , headGap
       , tailGap
       , headStyle
       , tailStyle
       , shaftStyle

         -- | See "Diagrams.TwoD.Arrowheads" for a list of standard
         --   arrowheads and help creating your own.
       , module Diagrams.TwoD.Arrowheads
       ) where

import           Control.Lens                     hiding (moveTo, ( # ), (<.>))
import           Data.AffineSpace
import           Data.Default.Class
import           Data.Functor                     ((<$>))
import           Data.Maybe                       (fromJust, fromMaybe)
import           Data.Monoid                      (mempty, (<>))
import           Data.VectorSpace
import           Diagrams.Core

import           Data.Colour                      hiding (atop)
import           Diagrams.Attributes
import           Diagrams.Parametric
import           Diagrams.Path
import           Diagrams.Solve                   (quadForm)
import           Diagrams.Tangent                 (tangentAtEnd, tangentAtStart)
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
    { _arrowHead  :: ArrowHT
    , _arrowTail  :: ArrowHT
    , _arrowShaft :: Trail R2
    , _headSize   :: Double
    , _tailSize   :: Double
    , _headGap    :: Double
    , _tailGap    :: Double
    , _headStyle  :: Style R2
    , _tailStyle  :: Style R2
    , _shaftStyle :: Style R2
    }

-- | Straight line arrow shaft.
straightShaft :: Trail R2
straightShaft = trailFromOffsets [unitX]

defShaftWidth :: Double
defShaftWidth = 0.03

instance Default ArrowOpts where
  def = ArrowOpts
        { _arrowHead    = dart
        , _arrowTail    = noTail
        , _arrowShaft   = trailFromOffsets [unitX]
        , _headSize     = 0.3
        , _tailSize     = 0.3
        , _headGap      = 0
        , _tailGap      = 0

        -- See note [Default arrow style attributes]
        , _headStyle    = mempty
        , _tailStyle    = mempty
        , _shaftStyle   = mempty
        }

makeLensesWith (lensRules & generateSignatures .~ False) ''ArrowOpts

-- | A shape to place at the head of the arrow.
arrowHead :: Lens' ArrowOpts ArrowHT

-- | A shape to place at the tail of the arrow.
arrowTail :: Lens' ArrowOpts ArrowHT

-- | The trail to use for the arrow shaft.
arrowShaft :: Lens' ArrowOpts (Trail R2)

-- | Radius of a circumcircle around the head.
headSize :: Lens' ArrowOpts Double

-- | Radius of a circumcircle around the tail.
tailSize :: Lens' ArrowOpts Double

-- | Distance to leave between the head and the target point.
headGap :: Lens' ArrowOpts Double
-- | Distance to leave between the starting point and the tail.
tailGap :: Lens' ArrowOpts Double

-- | Style to apply to the head. @headStyle@ is modified by using the lens
--   combinator @%~@ to change the current style. For example, to change
--   an opaque black arrowhead to translucent orange:
--   @(with & headStyle %~ fc orange .  opacity 0.75)@.
headStyle :: Lens' ArrowOpts (Style R2)

-- | Style to apply to the tail. See `headStyle`.
tailStyle :: Lens' ArrowOpts (Style R2)

-- | Style to apply to the shaft. See `headStyle`.
shaftStyle :: Lens' ArrowOpts (Style R2)

-- Set the default shaft style of an `ArrowOpts` record by applying the
-- default style after all other styles have been applied.
-- The semigroup stucture of the lw attribute will insure that the default
-- is only used if it has not been set in @opts@.
shaftSty :: ArrowOpts -> Style R2
shaftSty opts = lw defShaftWidth (opts^.shaftStyle)

-- Set the default head style. See `shaftSty`.
headSty :: ArrowOpts -> Style R2
headSty opts = fc black (opts^.headStyle)

-- Set the default tail style. See `shaftSty`.
tailSty :: ArrowOpts -> Style R2
tailSty opts = fc black (opts^.tailStyle)

-- | Calculate the length of the portion of the horizontal line that passes
--   through the origin and is inside of p.
xWidth :: (Traced t, V t ~ R2) => t -> Double
xWidth p = a + b
  where
    a = fromMaybe 0 (magnitude <$> traceV origin unitX p)
    b = fromMaybe 0 (magnitude <$> traceV origin unit_X p)

-- | Get the line color from the shaft to use as the fill color for the joint.
colorJoint :: Style v -> Style v
colorJoint sStyle =
    let c = fmap getLineColor . getAttr $ sStyle in
    case c of
        Nothing -> fillColor (black :: Colour Double)   -- default color for joints
                   $ mempty
        Just c' -> fillColor c' $ mempty

-- | Get line width from a style.
widthOfJoint :: Style v -> Double
widthOfJoint sStyle =
    let w = fmap getLineWidth . getAttr $ sStyle in
    case w of
        Nothing -> defShaftWidth -- this case should never happen.
        Just w' -> w'

-- | Combine the head and its joint into a single scale invariant diagram
--   and move the origin to the attachment point. Return the diagram
--   and its width.
mkHead :: Renderable (Path R2) b => ArrowOpts -> (Diagram b R2, Double)
mkHead opts = ( (j <> h) # moveOriginBy (jWidth *^ unit_X) # lw 0
              , hWidth + jWidth)
  where
    (h', j') = (opts^.arrowHead) (opts^.headSize) (widthOfJoint $ shaftSty opts)
    hWidth = xWidth h'
    jWidth = xWidth j'
    h = scaleInvPrim h' unitX # applyStyle (headSty opts)
    j = scaleInvPrim j' unitX # applyStyle (colorJoint (opts^.shaftStyle))

-- | Just like mkHead only the attachment point is on the right.
mkTail :: Renderable (Path R2) b => ArrowOpts -> (Diagram b R2, Double)
mkTail opts = ( (t <> j) # moveOriginBy (jWidth *^ unitX) # lw 0
              , tWidth + jWidth)
  where
    (t', j') = (opts^.arrowTail) (opts^.tailSize) (widthOfJoint $ shaftSty opts)
    tWidth = xWidth t'
    jWidth = xWidth j'
    t = scaleInvPrim t' unitX # applyStyle (tailSty opts)
    j = scaleInvPrim j' unitX # applyStyle (colorJoint (opts^.shaftStyle))

-- | Make a trail with the same angles and offset as an arrow with tail width
--   tw, head width hw and shaft of tr, such that the magnituted of the shaft
--   offset is size. Used for calculating the offset of an arrow.
spine :: Trail R2 -> Double -> Double -> Double -> Trail R2
spine tr tw hw size = tS <> tr # scale size <> hS
  where
    tSpine = trailFromOffsets [(normalized . tangentAtStart) $ tr] # scale tw
    hSpine = trailFromOffsets [(normalized . tangentAtEnd) $ tr] # scale hw
    hS = if hw > 0 then hSpine else mempty
    tS = if tw > 0 then tSpine else mempty

--  | Calculate the amount required to scale a shaft trail so that an arrow with
--    head width hw and tail width tw has offset t.
scaleFactor :: Trail R2 -> Double -> Double -> Double -> Double
scaleFactor tr tw hw t

  -- Let tv be a vector representing the tail width, i.e. a vector
  -- of length tw tangent to the trail's start; similarly for hv.
  -- Let v be the vector offset of the trail.
  --
  -- Then we want to find k such that
  --
  --   || tv + k*v + hv || = t.
  --
  -- We can solve by squaring both sides and expanding the LHS as a
  -- dot product, resulting in a quadratic in k.

  = case quadForm
             (magnitudeSq v)
             (2* (v <.> (tv ^+^ hv)))
             (magnitudeSq (tv ^+^ hv) - t*t)
    of
      []  -> 1   -- no scale works, just return 1
      [s] -> s   -- single solution
      ss  -> maximum ss
        -- we will usually get both a positive and a negative solution;
        -- return the maximum (i.e. positive) solution
  where
    tv = tw *^ (tangentAtStart tr # normalized)
    hv = hw *^ (tangentAtEnd   tr # normalized)
    v  = trailOffset tr

-- | @arrow len@ creates an arrow of length @len@ with default parameters.
arrow :: Renderable (Path R2) b => Double -> Diagram b R2
arrow len = arrow' def len

-- | @arrow' opts len@ creates an arrow of length @len@ using the
--   given options.  In particular, it scales the given 'arrowShaft'
--   so that the entire arrow has length @len@.
arrow' :: Renderable (Path R2) b => ArrowOpts -> Double -> Diagram b R2
arrow' opts len = dArrow # rotateBy (- dir)
  where
    -- Make the head and tail and save their widths.
    (h, hWidth') = mkHead opts
    (t, tWidth') = mkTail opts

    shaftTrail = opts^.arrowShaft

    -- Adjust the head width and tail width to take into accoung gaps
    tWidth = tWidth' + opts^.tailGap
    hWidth = hWidth' + opts^.headGap

    -- Calculate the angles that the head and tail should point.
    tAngle = direction . tangentAtStart $ shaftTrail :: Turn
    hAngle = direction . tangentAtEnd $ shaftTrail :: Turn

    -- Calculte the scaling factor to apply to the shaft shaftTrail so that the entire
    -- arrow will be of length len. Then apply it to the shaft and make the
    -- shaft into a Diagram with using it's style.
    sf = scaleFactor shaftTrail tWidth hWidth len
    shaftTrail' = shaftTrail # scale sf
    shaft = strokeT shaftTrail' # applyStyle (shaftSty opts)

    -- Adjust the head and tail to point in the directions of the shaft ends.
    h' = h # rotateBy hAngle
           # moveTo (origin .+^ shaftTrail' `atParam` domainUpper shaftTrail')
    t' = t # rotateBy tAngle

    -- Find out what direction the arrow is pointing so we can set it back
    -- to point in the direction unitX when we are done.
    dir = direction (trailOffset $ spine shaftTrail tWidth hWidth sf)

    -- Build the arrow and set it's origin at the start.
    dArrow = moveOriginBy ((tWidth *^ (unit_X # rotateBy tAngle))) $ h' <> t' <> shaft

-- | @arrowBetween s e@ creates an arrow pointing from @s@ to @e@
--   with default parameters.
arrowBetween :: Renderable (Path R2) b => P2 -> P2 -> Diagram b R2
arrowBetween = arrowBetween' def

-- | @arrowBetween' opts s e@ creates an arrow pointing from @s@ to
--   @e@ using the given options.  In particular, it scales and
--   rotates @arrowShaft@ to go between @s@ and @e@, taking head,
--   tail, and gaps into account.
arrowBetween'
  :: Renderable (Path R2) b =>
     ArrowOpts -> P2 -> P2 -> Diagram b R2
arrowBetween' opts s e = arrowAt' opts s (e .-. s)

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

-- | Connect two diagrams at point on the perimeter of the diagrams, choosen
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

-- | Draw an arrow from diagram named "n1" to diagram named "n2".  The
--   arrow lies on the line between the centres of the diagrams, but is
--   drawn so that it stops at the boundaries of the diagrams, using traces
--   to find the intersection points.
connectOutside
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
  => n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connectOutside = connectOutside' def

connectOutside'
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
  => ArrowOpts -> n1 -> n2 -> (Diagram b R2 -> Diagram b R2)
connectOutside' opts n1 n2 =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
    let v = location b2 .-. location b1
        midpoint = location b1 .+^ (v/2)
        s = fromJust $ traceP midpoint (-v) b1
        e = fromJust $ traceP midpoint v b2
    in atop (arrowBetween' opts s e)
