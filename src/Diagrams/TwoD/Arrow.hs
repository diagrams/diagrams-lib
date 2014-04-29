{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

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
       ( -- * Examples
         -- ** Example 1
-- | <<diagrams/src_Diagrams_TwoD_Arrow_example1.svg#diagram=example1&width=500>>
--
--   > -- Connecting two diagrams at their origins.
--   >
--   > sq = square 2 # showOrigin # lc darkgray # lw 0.07
--   > ds = (sq # named "left") ||| strutX 3 ||| (sq # named "right")
--   >
--   > shaft  = cubicSpline False ( map p2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])
--   >
--   > example1 = ds # connect' (with & arrowHead .~ dart & arrowTail .~ quill
--   >                                & shaftStyle %~ lw 0.02 & arrowShaft .~ shaft)
--   >                                "left" "right" # pad 1.1

         -- ** Example 2

-- | <<diagrams/src_Diagrams_TwoD_Arrow_example2.svg#diagram=example2&width=500>>
--
--   > -- Comparing connect, connectPerim, and arrowAt.
--   >
--   > oct  = octagon 1 # lc darkgray # lw 0.050 # showOrigin
--   > dias = oct # named "first" ||| strut 3 ||| oct # named "second"
--   >
--   > -- Connect two diagrams and two points on their trails.
--   > ex12 = dias # connect "first" "second"
--   >             # connectPerim "first" "second" (15/16 \@\@ turn) (9/16 \@\@ turn)
--   >
--   > -- Place an arrow at (0,0) the size and direction of (0,1).
--   > ex3 = arrowAt origin unit_Y
--   >
--   > example2 = (ex12 <> ex3) # centerXY # pad 1.1

         -- * Creating arrows
         arrowV
       , arrowV'
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

       , arrow
       , arrow'

         -- * Options
       , ArrowOpts(..)

       , arrowHead
       , arrowTail
       , arrowShaft
       , headGap
       , tailGap
       , gaps, gap
       , headColor
       , headTexture
       , headStyle
       , headSize
       , tailColor
       , tailTexture
       , tailStyle
       , tailSize
       , shaftColor
       , shaftTexture
       , shaftStyle
       , straightShaft

         -- | See "Diagrams.TwoD.Arrowheads" for a list of standard
         --   arrowheads and help creating your own.
       , module Diagrams.TwoD.Arrowheads
       ) where

import           Control.Applicative      ((<*>))
import           Control.Lens             (Lens', Setter', Traversal',
                                           generateSignatures, lensRules,
                                           makeLensesWith, (%~), (&), (.~),
                                           (^.))
import           Data.AffineSpace
import           Data.Default.Class
import           Data.Functor             ((<$>))
import           Data.Maybe               (fromMaybe)
import           Data.Monoid.Coproduct    (untangle)
import           Data.Semigroup
import           Data.VectorSpace

import           Data.Colour              hiding (atop)
import           Diagrams.Core
import           Diagrams.Core.Types      (QDiaLeaf (..), mkQD')

import           Diagrams.Angle
import           Diagrams.Parametric
import           Diagrams.Path
import           Diagrams.Solve           (quadForm)
import           Diagrams.Tangent         (tangentAtEnd, tangentAtStart)
import           Diagrams.Trail
import           Diagrams.Attributes
import           Diagrams.TwoD.Arrowheads
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Path       (stroke, strokeT)
import           Diagrams.TwoD.Transform  (rotate, translateX)
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector     (direction, unitX, unit_X)
import           Diagrams.Util            (( # ))

data ArrowOpts
  = ArrowOpts
    { _arrowHead  :: ArrowHT
    , _arrowTail  :: ArrowHT
    , _arrowShaft :: Trail R2
    , _headGap    :: Double
    , _tailGap    :: Double
    , _headStyle  :: Style R2
    , _headSize   :: Measure R2
    , _tailStyle  :: Style R2
    , _tailSize   :: Measure R2
    , _shaftStyle :: Style R2
    }

-- | Straight line arrow shaft.
straightShaft :: Trail R2
straightShaft = trailFromOffsets [unitX]

instance Default ArrowOpts where
  def = ArrowOpts
        { _arrowHead    = dart
        , _arrowTail    = noTail
        , _arrowShaft   = straightShaft
        , _headGap      = 0
        , _tailGap      = 0

        -- See note [Default arrow style attributes]
        , _headStyle    = mempty
        , _headSize     = normal
        , _tailStyle    = mempty
        , _tailSize     = normal
        , _shaftStyle   = mempty
        }

makeLensesWith (lensRules & generateSignatures .~ False) ''ArrowOpts

-- | A shape to place at the head of the arrow.
arrowHead :: Lens' ArrowOpts ArrowHT

-- | A shape to place at the tail of the arrow.
arrowTail :: Lens' ArrowOpts ArrowHT

-- | The trail to use for the arrow shaft.
arrowShaft :: Lens' ArrowOpts (Trail R2)

-- | Distance to leave between the head and the target point.
headGap :: Lens' ArrowOpts Double

-- | Distance to leave between the starting point and the tail.
tailGap :: Lens' ArrowOpts Double

-- | Set both the @headGap@ and @tailGap@ simultaneously.
gaps :: Traversal' ArrowOpts Double
gaps f opts = (\h t -> opts & headGap .~ h & tailGap .~ t) <$> f (opts ^. headGap) <*> f (opts ^. tailGap)

-- | Same as gaps, provided for backward compatiiblity.
gap :: Traversal' ArrowOpts Double
gap = gaps

-- | Style to apply to the head. @headStyle@ is modified by using the lens
--   combinator @%~@ to change the current style. For example, to change
--   an opaque black arrowhead to translucent orange:
--   @(with & headStyle %~ fc orange .  opacity 0.75)@.
headStyle :: Lens' ArrowOpts (Style R2)

-- | Style to apply to the tail. See `headStyle`.
tailStyle :: Lens' ArrowOpts (Style R2)

-- | Style to apply to the shaft. See `headStyle`.
shaftStyle :: Lens' ArrowOpts (Style R2)

-- | The radius of the circumcircle around the head.
headSize :: Lens' ArrowOpts (Measure R2)

-- | The radius of the circumcircle around the tail.
tailSize :: Lens' ArrowOpts (Measure R2)

-- | A lens for setting or modifying the color of an arrowhead. For
--   example, one may write @... (with & headColor .~ blue)@ to get an
--   arrow with a blue head, or @... (with & headColor %~ blend 0.5
--   white)@ to make an arrow's head a lighter color.  For more general
--   control over the style of arrowheads, see 'headStyle'.
--
--   Note that the most general type of @headColor@ would be
--
-- @
--   (Color c, Color c') => Setter ArrowOpts ArrowOpts c c'
-- @
--
--   but that can cause problems for type inference when setting the
--   color.  However, using it at that more general type may
--   occasionally be useful, for example, if you want to apply some
--   opacity to a color, as in @... (with & headColor %~
--   (\`withOpacity\` 0.5))@.  If you want the more general type, you
--   can use @'headStyle' . 'styleFillColor'@ in place of @headColor@.
headColor :: Color c => Setter' ArrowOpts c
headColor = headStyle . styleFillColor

-- | A lens for setting or modifying the color of an arrow
--   tail. See 'headColor'.
tailColor :: Color c => Setter' ArrowOpts c
tailColor = tailStyle . styleFillColor

-- | A lens for setting or modifying the color of an arrow
--   shaft. See 'headColor'.
shaftColor :: Color c => Setter' ArrowOpts c
shaftColor = shaftStyle . styleLineColor

-- | A lens for setting or modifying the texture of an arrow
--   head.
headTexture :: Setter' ArrowOpts Texture
headTexture = headStyle . styleFillTexture

-- | A lens for setting or modifying the texture of an arrow
--   tail.
tailTexture :: Setter' ArrowOpts Texture
tailTexture = tailStyle . styleFillTexture

-- | A lens for setting or modifying the texture of an arrow
--   shaft.
shaftTexture :: Setter' ArrowOpts Texture
shaftTexture = shaftStyle . styleLineTexture

-- Set the default shaft style of an `ArrowOpts` record by applying the
-- default style after all other styles have been applied.
-- The semigroup stucture of the lw attribute will insure that the default
-- is only used if it has not been set in @opts@.
shaftSty :: ArrowOpts -> Style R2
shaftSty opts = opts^.shaftStyle

-- Set the default head style. See `shaftSty`.
headSty :: ArrowOpts -> Style R2
headSty opts = fc black (opts^.headStyle)

-- Set the default tail style. See `shaftSty`.
tailSty :: ArrowOpts -> Style R2
tailSty opts = fc black (opts^.tailStyle)

fromMeasure :: Double -> Double -> Measure R2 -> Double
fromMeasure g n m = u
  where Output u = toOutput g n m

-- | Calculate the length of the portion of the horizontal line that passes
--   through the origin and is inside of p.
xWidth :: (Traced t, V t ~ R2) => t -> Double
xWidth p = a + b
  where
    a = fromMaybe 0 (magnitude <$> traceV origin unitX p)
    b = fromMaybe 0 (magnitude <$> traceV origin unit_X p)

-- | Get the line color from the shaft to use as the fill color for the joint.
colorJoint :: Style R2 -> Style R2
colorJoint sStyle =
    let c = fmap getLineColor . getAttr $ sStyle in
    case c of
        Nothing -> fillColor (black :: Colour Double)   -- default color for joints
                   $ mempty
        Just c' -> fillColor c' $ mempty

-- | Get line width from a style.
widthOfJoint :: Style v -> Double -> Double  -> Double
widthOfJoint sStyle gToO nToO =
  maybe (fromMeasure gToO nToO (Output 1)) -- Should be same as default line width
        (fromMeasure gToO nToO)
        (fmap getLineWidth . getAttr $ sStyle)

-- | Combine the head and its joint into a single scale invariant diagram
--   and move the origin to the attachment point. Return the diagram
--   and its width.
mkHead :: Renderable (Path R2) b =>
          Double -> ArrowOpts -> Double -> Double -> (Diagram b R2, Double)
mkHead size opts gToO nToO = ((j <> h) # moveOriginBy (jWidth *^ unit_X) # lwO 0
              , hWidth + jWidth)
  where
    (h', j') = (opts^.arrowHead) size
               (widthOfJoint (shaftSty opts) gToO nToO)
    hWidth = xWidth h'
    jWidth = xWidth j'
    h = stroke h' # applyStyle (headSty opts)
    j = stroke j' # applyStyle (colorJoint (opts^.shaftStyle))

-- | Just like mkHead only the attachment point is on the right.
mkTail :: Renderable (Path R2) b =>
          Double -> ArrowOpts -> Double -> Double -> (Diagram b R2, Double)
mkTail size opts gToO nToO = ((t <> j) # moveOriginBy (jWidth *^ unitX) # lwO 0
              , tWidth + jWidth)
  where
    (t', j') = (opts^.arrowTail) size
               (widthOfJoint (shaftSty opts) gToO nToO)
    tWidth = xWidth t'
    jWidth = xWidth j'
    t = stroke t' # applyStyle (tailSty opts)
    j = stroke j' # applyStyle (colorJoint (opts^.shaftStyle))

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

-- Calculate the approximate envelope of a horizontal arrow
-- as if the arrow were made only of a shaft.
arrowEnv :: ArrowOpts -> Double -> Envelope R2
arrowEnv opts len = getEnvelope horizShaft
  where
    horizShaft = shaft # rotate (negateV direction v) # scale (len / m)
    m = magnitude v
    v = trailOffset shaft
    shaft = opts ^. arrowShaft

-- | @arrow len@ creates an arrow of length @len@ with default
--   parameters, starting at the origin and ending at the point
--   @(len,0)@.
arrow :: Renderable (Path R2) b => Double -> Diagram b R2
arrow len = arrow' def len

-- | @arrow' opts len@ creates an arrow of length @len@ using the
--   given options, starting at the origin and ending at the point
--   @(len,0)@.  In particular, it scales the given 'arrowShaft' so
--   that the entire arrow has length @len@.
arrow' :: Renderable (Path R2) b => ArrowOpts -> Double -> Diagram b R2
arrow' opts len = mkQD' (DelayedLeaf delayedArrow)

      -- Currently arrows have an empty envelope and trace.
      (arrowEnv opts len) mempty mempty mempty

  where

    -- Once we learn the global transformation context (da) and the two scale
    -- factors, normal to output (n) and global to output (g), this arrow is
    -- drawn in, we can apply it to the origin and (len,0) to find out
    -- the actual final points between which this arrow should be
    -- drawn.  We need to know this to draw it correctly, since the
    -- head and tail are scale invariant, and hence the precise points
    -- between which we need to draw the shaft do not transform
    -- uniformly as the transformation applied to the entire arrow.
    -- See https://github.com/diagrams/diagrams-lib/issues/112.
    delayedArrow da g n =
      let (trans, globalSty) = option mempty untangle . fst $ da
      in  dArrow globalSty trans len g n

    -- Build an arrow and set its endpoints to the image under tr of origin and (len,0).
    dArrow sty tr ln gToO nToO = (h' <> t' <> shaft)
               # moveOriginBy (tWidth *^ (unit_X # rotate tAngle))
               # rotate (direction (q .-. p) ^-^ dir)
               # moveTo p
      where

        p = origin # transform tr
        q = origin # translateX ln # transform tr

        -- Use the existing line color for head, tail, and shaft by
        -- default (can be overridden by explicitly setting headStyle,
        -- tailStyle, or shaftStyle).
        globalLC = getLineColor <$> getAttr sty
        opts' = opts
          & headStyle  %~ maybe id fillColor globalLC
          & tailStyle  %~ maybe id fillColor globalLC
          & shaftStyle %~ maybe id lineColor globalLC

        -- The head size is obtained from the style and converted to output
        -- units.
        hSize = fromMeasure gToO nToO (opts ^. headSize)

        -- The tail size is obtained from the style and converted to output
        -- units.
        tSize = fromMeasure gToO nToO (opts ^. tailSize)

        -- Make the head and tail and save their widths.
        (h, hWidth') = mkHead hSize opts' gToO nToO
        (t, tWidth') = mkTail tSize opts' gToO nToO

        rawShaftTrail = opts^.arrowShaft
        shaftTrail
          = rawShaftTrail
            -- rotate it so it is pointing in the positive X direction
          # rotate (negateV direction (trailOffset rawShaftTrail))
            -- apply the context transformation -- in case it includes
            -- things like flips and shears (the possibility of shears
            -- is why we must rotate it to a neutral position first)
          # transform tr

        -- Adjust the head width and tail width to take gaps into account
        tWidth = tWidth' + opts^.tailGap
        hWidth = hWidth' + opts^.headGap

        -- Calculate the angles that the head and tail should point.
        tAngle = direction . tangentAtStart $ shaftTrail
        hAngle = direction . tangentAtEnd $ shaftTrail

        -- Calculte the scaling factor to apply to the shaft shaftTrail so that the entire
        -- arrow will be of length len. Then apply it to the shaft and make the
        -- shaft into a Diagram with using its style.
        sf = scaleFactor shaftTrail tWidth hWidth (magnitude (q .-. p))
        shaftTrail' = shaftTrail # scale sf
        shaft = strokeT shaftTrail' # applyStyle (shaftSty opts)

        -- Adjust the head and tail to point in the directions of the shaft ends.
        h' = h # rotate hAngle
               # moveTo (origin .+^ shaftTrail' `atParam` domainUpper shaftTrail')
        t' = t # rotate tAngle

        -- Find out what direction the arrow is pointing so we can set it back
        -- to point in the direction unitX when we are done.
        dir = direction (trailOffset $ spine shaftTrail tWidth hWidth sf)

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
--   the vector v.
arrowAt :: Renderable (Path R2) b => P2 -> R2 -> Diagram b R2
arrowAt s v = arrowAt' def s v

arrowAt'
  :: Renderable (Path R2) b =>
     ArrowOpts -> P2 -> R2 -> Diagram b R2
arrowAt' opts s v = arrow' opts len
                  # rotate dir # moveTo s
  where
    len = magnitude v
    dir = direction v

-- | @arrowV v@ creates an arrow with the direction and magnitude of
--   the vector @v@ (with its tail at the origin), using default
--   parameters.
arrowV :: Renderable (Path R2) b => R2 -> Diagram b R2
arrowV = arrowV' def

-- | @arrowV' v@ creates an arrow with the direction and magnitude of
--   the vector @v@ (with its tail at the origin).
arrowV'
  :: Renderable (Path R2) b
  => ArrowOpts -> R2 -> Diagram b R2
arrowV' opts = arrowAt' opts origin

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
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
 => n1 -> n2 -> Angle -> Angle
  -> (Diagram b R2 -> Diagram b R2)
connectPerim = connectPerim' def

connectPerim'
  :: (Renderable (Path R2) b, IsName n1, IsName n2)
  => ArrowOpts -> n1 -> n2 -> Angle -> Angle
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
        s' = fromMaybe (location b1) $ traceP midpoint (-v) b1
        e' = fromMaybe (location b2) $ traceP midpoint v b2
    in
      atop (arrowBetween' opts s' e')
