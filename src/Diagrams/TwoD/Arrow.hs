{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

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
--   > sq = square 2 # showOrigin # lc darkgray # lw ultraThick
--   > ds = (sq # named "left") ||| strutX 3 ||| (sq # named "right")
--   >
--   > shaft  = cubicSpline False ( map p2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])
--   >
--   > example1 = ds # connect' (with & arrowHead .~ dart & arrowTail .~ quill
--   >                                & arrowShaft .~ shaft
--   >                                & headLength .~ huge & tailLength .~ veryLarge)
--   >                                "left" "right" # pad 1.1

         -- ** Example 2

-- | <<diagrams/src_Diagrams_TwoD_Arrow_example2.svg#diagram=example2&width=500>>
--
--   > -- Comparing connect, connectPerim, and arrowAt.
--   >
--   > oct  = octagon 1 # lc darkgray # lw ultraThick # showOrigin
--   > dias = oct # named "first" ||| strut 3 ||| oct # named "second"
--   >
--   > -- Connect two diagrams and two points on their trails.
--   > ex12 = dias # connect' (with & lengths .~ veryLarge) "first" "second"
--   >             # connectPerim' (with & lengths .~ veryLarge)
--   >        "first" "second" (15/16 @@ turn) (9/16 @@ turn)
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
       , headTexture
       , headStyle
       , headLength
       , tailTexture
       , tailStyle
       , tailLength
       , lengths
       , shaftTexture
       , shaftStyle
       , straightShaft

         -- | See "Diagrams.TwoD.Arrowheads" for a list of standard
         --   arrowheads and help creating your own.
       , module Diagrams.TwoD.Arrowheads
       ) where

import           Control.Applicative      ((<*>))
import           Control.Lens             (Lens', Setter', Traversal', generateSignatures,
                                           lensRules, makeLensesWith, view, (%~), (&), (.~), (^.))
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
import           Diagrams.Attributes
import           Diagrams.Direction
import           Diagrams.Parametric
import           Diagrams.Path
import           Diagrams.Solve           (quadForm)
import           Diagrams.Tangent         (tangentAtEnd, tangentAtStart)
import           Diagrams.Trail
import           Diagrams.TwoD.Arrowheads
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Path       (stroke, strokeT)
import           Diagrams.TwoD.Transform  (rotate, translateX)
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector     (unitX, unit_X)
import           Diagrams.Util            (( # ))

data ArrowOpts v
  = ArrowOpts
    { _arrowHead  :: ArrowHT v
    , _arrowTail  :: ArrowHT v
    , _arrowShaft :: Trail v
    , _headGap    :: Measure v
    , _tailGap    :: Measure v
    , _headStyle  :: Style v
    , _headLength :: Measure v
    , _tailStyle  :: Style v
    , _tailLength :: Measure v
    , _shaftStyle :: Style v
    }

-- | Straight line arrow shaft.
straightShaft :: (R2Ish v) => Trail v
straightShaft = trailFromOffsets [unitX]

instance (R2Ish v) => Default (ArrowOpts v) where
  def = ArrowOpts
        { _arrowHead    = dart
        , _arrowTail    = noTail
        , _arrowShaft   = straightShaft
        , _headGap      = none
        , _tailGap      = none

        -- See note [Default arrow style attributes]
        , _headStyle    = mempty
        , _headLength     = normal
        , _tailStyle    = mempty
        , _tailLength     = normal
        , _shaftStyle   = mempty
        }

makeLensesWith (lensRules & generateSignatures .~ False) ''ArrowOpts

-- | A shape to place at the head of the arrow.
arrowHead :: Lens' (ArrowOpts v) (ArrowHT v)

-- | A shape to place at the tail of the arrow.
arrowTail :: Lens' (ArrowOpts v) (ArrowHT v)

-- | The trail to use for the arrow shaft.
arrowShaft :: Lens' (ArrowOpts v) (Trail v)

-- | Distance to leave between the head and the target point.
headGap :: Lens' (ArrowOpts v) (Measure v)

-- | Distance to leave between the starting point and the tail.
tailGap :: Lens' (ArrowOpts v) (Measure v)

-- | Set both the @headGap@ and @tailGap@ simultaneously.
gaps :: Traversal' (ArrowOpts v) (Measure v)
gaps f opts = (\h t -> opts & headGap .~ h & tailGap .~ t)
        <$> f (opts ^. headGap)
        <*> f (opts ^. tailGap)

-- | Same as gaps, provided for backward compatiiblity.
gap :: Traversal' (ArrowOpts v) (Measure v)
gap = gaps

-- | Style to apply to the head. @headStyle@ is modified by using the lens
--   combinator @%~@ to change the current style. For example, to change
--   an opaque black arrowhead to translucent orange:
--   @(with & headStyle %~ fc orange .  opacity 0.75)@.
headStyle :: Lens' (ArrowOpts v) (Style v)

-- | Style to apply to the tail. See `headStyle`.
tailStyle :: Lens' (ArrowOpts v) (Style v)

-- | Style to apply to the shaft. See `headStyle`.
shaftStyle :: Lens' (ArrowOpts v) (Style v)

-- | The length from the start of the joint to the tip of the head.
headLength :: Lens' (ArrowOpts v) (Measure v)

-- | The length of the tail plus its joint.
tailLength :: Lens' (ArrowOpts v) (Measure v)

-- | Set both the @headLength@ and @tailLength@ simultaneously.
lengths :: Traversal' (ArrowOpts v) (Measure v)
lengths f opts = (\h t -> opts & headLength .~ h & tailLength .~ t) <$> f (opts ^. headLength)
             <*> f (opts ^. tailLength)

-- | A lens for setting or modifying the texture of an arrowhead. For
--   example, one may write @... (with & headTexture .~ grad)@ to get an
--   arrow with a head filled with a gradient, assuming grad has been
--   defined. Or @... (with & headTexture .~ solid blue@ to set the head
--   color to blue. For more general control over the style of arrowheads,
--   see 'headStyle'.
headTexture :: (R2Ish v) => Setter' (ArrowOpts v) (Texture v)
headTexture = headStyle . styleFillTexture

-- | A lens for setting or modifying the texture of an arrow
--   tail.
tailTexture :: (R2Ish v) => Setter' (ArrowOpts v) (Texture v)
tailTexture = tailStyle . styleFillTexture

-- | A lens for setting or modifying the texture of an arrow
--   shaft.
shaftTexture :: (R2Ish v) => Setter' (ArrowOpts v) (Texture v)
shaftTexture = shaftStyle . styleLineTexture

-- Set the default shaft style of an `ArrowOpts` record by applying the
-- default style after all other styles have been applied.
-- The semigroup stucture of the lw attribute will insure that the default
-- is only used if it has not been set in @opts@.
shaftSty :: (R2Ish v) => ArrowOpts v -> Style v
shaftSty opts = opts^.shaftStyle

-- Set the default head style. See `shaftSty`.
headSty :: (R2Ish v) => ArrowOpts v -> Style v
headSty opts = fc black (opts^.headStyle)

-- Set the default tail style. See `shaftSty`.
tailSty :: (R2Ish v) => ArrowOpts v -> Style v
tailSty opts = fc black (opts^.tailStyle)

fromMeasure :: (R2Ish v) => Scalar v -> Scalar v -> Measure v -> Scalar v
fromMeasure g n m = u
  where Output u = toOutput g n m

-- | Calculate the length of the portion of the horizontal line that passes
--   through the origin and is inside of p.
xWidth :: (R2Ish v) => (Traced t, V t ~ v) => t -> Scalar v
xWidth p = a + b
  where
    a = fromMaybe 0 (magnitude <$> traceV origin unitX p)
    b = fromMaybe 0 (magnitude <$> traceV origin unit_X p)

-- | Get the line color from the shaft to use as the fill color for the joint.
--   And set the opacity of the shaft to the current opacity.
colorJoint :: (R2Ish v) => Style v -> Style v
colorJoint sStyle =
    let c = fmap getLineTexture . getAttr $ sStyle
        o = fmap getOpacity . getAttr $ sStyle
    in
    case (c, o) of
        (Nothing, Nothing) -> fillColor (black :: Colour Double) $ mempty
        (Just t, Nothing) -> fillTexture t $ mempty
        (Nothing, Just o') -> opacity o' . fillColor (black :: Colour Double)  $ mempty
        (Just t, Just o') -> opacity o' . fillTexture t $ mempty

-- | Get line width from a style.
widthOfJoint :: forall v. (R2Ish v) => Style v -> Scalar v -> Scalar v  -> Scalar v
widthOfJoint sStyle gToO nToO =
  maybe (fromMeasure gToO nToO (Output 1 :: Measure v)) -- Should be same as default line width
        (fromMeasure gToO nToO)
        (fmap getLineWidth . getAttr $ sStyle :: Maybe (Measure v))

-- | Combine the head and its joint into a single scale invariant diagram
--   and move the origin to the attachment point. Return the diagram
--   and its width.
mkHead :: (R2Ish v, Renderable (Path v) b) =>
          Scalar v -> ArrowOpts v -> Scalar v -> Scalar v -> (Diagram b v, Scalar v)
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
mkTail :: (R2Ish v, Renderable (Path v) b) =>
          Scalar v -> ArrowOpts v -> Scalar v -> Scalar v -> (Diagram b v, Scalar v)
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
spine :: (R2Ish v) => Trail v -> Scalar v -> Scalar v -> Scalar v -> Trail v
spine tr tw hw size = tS <> tr # scale size <> hS
  where
    tSpine = trailFromOffsets [(normalized . tangentAtStart) $ tr] # scale tw
    hSpine = trailFromOffsets [(normalized . tangentAtEnd) $ tr] # scale hw
    hS = if hw > 0 then hSpine else mempty
    tS = if tw > 0 then tSpine else mempty

--  | Calculate the amount required to scale a shaft trail so that an arrow with
--    head width hw and tail width tw has offset t.
scaleFactor :: (R2Ish v) => Trail v -> Scalar v -> Scalar v -> Scalar v -> Scalar v
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
arrowEnv :: (R2Ish v) => ArrowOpts v -> Scalar v -> Envelope v
arrowEnv opts len = getEnvelope horizShaft
  where
    horizShaft = shaft # rotate (negateV (v ^. _theta)) # scale (len / m)
    m = magnitude v
    v = trailOffset shaft
    shaft = opts ^. arrowShaft

-- | @arrow len@ creates an arrow of length @len@ with default
--   parameters, starting at the origin and ending at the point
--   @(len,0)@.
arrow :: (R2Ish v, Renderable (Path v) b) => Scalar v -> Diagram b v
arrow len = arrow' def len

-- | @arrow' opts len@ creates an arrow of length @len@ using the
--   given options, starting at the origin and ending at the point
--   @(len,0)@.  In particular, it scales the given 'arrowShaft' so
--   that the entire arrow has length @len@.
arrow' :: (R2Ish v, Renderable (Path v) b) => ArrowOpts v -> Scalar v -> Diagram b v
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
               # rotate (((q .-. p)^._theta) ^-^ (dir^._theta))
               # moveTo p
      where

        p = origin # transform tr
        q = origin # translateX ln # transform tr

        -- Use the existing line color for head, tail, and shaft by
        -- default (can be overridden by explicitly setting headStyle,
        -- tailStyle, or shaftStyle).
        globalLC = getLineTexture <$> getAttr sty
        opts' = opts
          & headStyle  %~ maybe id fillTexture globalLC
          & tailStyle  %~ maybe id fillTexture globalLC
          & shaftStyle %~ maybe id lineTexture globalLC

        -- The head size, tail size, head gap, and tail gap are obtained
        -- from the style and converted to output units.
        hSize = fromMeasure gToO nToO . transform tr $ opts ^. headLength
        tSize = fromMeasure gToO nToO . transform tr $ opts ^. tailLength
        hGap = fromMeasure gToO nToO . transform tr $ opts ^. headGap
        tGap = fromMeasure gToO nToO . transform tr $ opts ^. tailGap

        -- Make the head and tail and save their widths.
        (h, hWidth') = mkHead hSize opts' gToO nToO
        (t, tWidth') = mkTail tSize opts' gToO nToO

        rawShaftTrail = opts^.arrowShaft
        shaftTrail
          = rawShaftTrail
            -- rotate it so it is pointing in the positive X direction
          # rotate (negateV . view _theta . trailOffset $ rawShaftTrail)
            -- apply the context transformation -- in case it includes
            -- things like flips and shears (the possibility of shears
            -- is why we must rotate it to a neutral position first)
          # transform tr

        -- Adjust the head width and tail width to take gaps into account
        tWidth = tWidth' + tGap
        hWidth = hWidth' + hGap

        -- Calculate the angles that the head and tail should point.
        tAngle = tangentAtStart shaftTrail ^. _theta
        hAngle = tangentAtEnd shaftTrail ^. _theta

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
arrowBetween :: (R2Ish v, Renderable (Path v) b) => Point v -> Point v -> Diagram b v
arrowBetween = arrowBetween' def

-- | @arrowBetween' opts s e@ creates an arrow pointing from @s@ to
--   @e@ using the given options.  In particular, it scales and
--   rotates @arrowShaft@ to go between @s@ and @e@, taking head,
--   tail, and gaps into account.
arrowBetween'
  :: (R2Ish v, Renderable (Path v) b) =>
     ArrowOpts v -> Point v -> Point v -> Diagram b v
arrowBetween' opts s e = arrowAt' opts s (e .-. s)

-- | Create an arrow starting at s with length and direction determined by
--   the vector v.
arrowAt :: (R2Ish v, Renderable (Path v) b) => Point v -> v -> Diagram b v
arrowAt s v = arrowAt' def s v

arrowAt'
  :: (R2Ish v, Renderable (Path v) b) =>
     ArrowOpts v -> Point v -> v -> Diagram b v
arrowAt' opts s v = arrow' opts len
                  # rotate dir # moveTo s
  where
    len = magnitude v
    dir = v ^. _theta

-- | @arrowV v@ creates an arrow with the direction and magnitude of
--   the vector @v@ (with its tail at the origin), using default
--   parameters.
arrowV :: (R2Ish v, Renderable (Path v) b) => v -> Diagram b v
arrowV = arrowV' def

-- | @arrowV' v@ creates an arrow with the direction and magnitude of
--   the vector @v@ (with its tail at the origin).
arrowV'
  :: (R2Ish v, Renderable (Path v) b)
  => ArrowOpts v -> v -> Diagram b v
arrowV' opts = arrowAt' opts origin

-- | Connect two diagrams with a straight arrow.
connect
  :: (R2Ish v, Renderable (Path v) b, IsName n1, IsName n2)
  => n1 -> n2 -> (Diagram b v -> Diagram b v)
connect = connect' def

-- | Connect two diagrams with an arbitrary arrow.
connect'
  :: (R2Ish v, Renderable (Path v) b, IsName n1, IsName n2)
  => ArrowOpts v -> n1 -> n2 -> (Diagram b v -> Diagram b v)
connect' opts n1 n2 =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [s,e] = map location [sub1, sub2]
    in  atop (arrowBetween' opts s e)

-- | Connect two diagrams at point on the perimeter of the diagrams, choosen
--   by angle.
connectPerim
  :: (R2Ish v, Renderable (Path v) b, IsName n1, IsName n2)
 => n1 -> n2 -> Angle (Scalar v) -> Angle (Scalar v)
  -> (Diagram b v -> Diagram b v)
connectPerim = connectPerim' def

connectPerim'
  :: (R2Ish v, Renderable (Path v) b, IsName n1, IsName n2)
  => ArrowOpts v -> n1 -> n2 -> Angle (Scalar v) -> Angle (Scalar v)
  -> (Diagram b v -> Diagram b v)
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
  :: (R2Ish v, Renderable (Path v) b, IsName n1, IsName n2)
  => n1 -> n2 -> (Diagram b v -> Diagram b v)
connectOutside = connectOutside' def

connectOutside'
  :: (R2Ish v, Renderable (Path v) b, IsName n1, IsName n2)
  => ArrowOpts v -> n1 -> n2 -> (Diagram b v -> Diagram b v)
connectOutside' opts n1 n2 =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
    let v = location b2 .-. location b1
        midpoint = location b1 .+^ (v ^/ 2)
        s' = fromMaybe (location b1) $ traceP midpoint (negateV v) b1
        e' = fromMaybe (location b2) $ traceP midpoint v b2
    in
      atop (arrowBetween' opts s' e')
