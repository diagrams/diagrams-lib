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
       
       , arrowFromLocatedTrail
       , arrowFromLocatedTrail'

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
import           Data.Default.Class
import           Data.Functor             ((<$>))
import           Data.Maybe               (fromMaybe)
import           Data.Monoid.Coproduct    (untangle)
import           Data.Semigroup

import           Data.Colour              hiding (atop)
import           Diagrams.Core
import           Diagrams.Core.Types      (QDiaLeaf (..), mkQD')

import           Diagrams.Angle
import           Diagrams.Attributes
import           Diagrams.Direction
import           Diagrams.Located         (Located(..), unLoc)
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

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector


data ArrowOpts n
  = ArrowOpts
    { _arrowHead  :: ArrowHT n
    , _arrowTail  :: ArrowHT n
    , _arrowShaft :: Trail V2 n
    , _headGap    :: Measure n
    , _tailGap    :: Measure n
    , _headStyle  :: Style V2 n
    , _headLength :: Measure n
    , _tailStyle  :: Style V2 n
    , _tailLength :: Measure n
    , _shaftStyle :: Style V2 n
    }

-- | Straight line arrow shaft.
straightShaft :: OrderedField n => Trail V2 n
straightShaft = trailFromOffsets [unitX]

instance RealFloat n => Default (ArrowOpts n) where
  def = ArrowOpts
        { _arrowHead    = dart
        , _arrowTail    = noTail
        , _arrowShaft   = straightShaft
        , _headGap      = none
        , _tailGap      = none

        -- See note [Default arrow style attributes]
        , _headStyle    = mempty
        , _headLength   = normal
        , _tailStyle    = mempty
        , _tailLength   = normal
        , _shaftStyle   = mempty
        }

makeLensesWith (lensRules & generateSignatures .~ False) ''ArrowOpts

-- | A shape to place at the head of the arrow.
arrowHead :: Lens' (ArrowOpts n) (ArrowHT n)

-- | A shape to place at the tail of the arrow.
arrowTail :: Lens' (ArrowOpts n) (ArrowHT n)

-- | The trail to use for the arrow shaft.
arrowShaft :: Lens' (ArrowOpts n) (Trail V2 n)

-- | Distance to leave between the head and the target point.
headGap :: Lens' (ArrowOpts n) (Measure n)

-- | Distance to leave between the starting point and the tail.
tailGap :: Lens' (ArrowOpts n) (Measure n)

-- | Set both the @headGap@ and @tailGap@ simultaneously.
gaps :: Traversal' (ArrowOpts n) (Measure n)
gaps f opts = (\h t -> opts & headGap .~ h & tailGap .~ t)
        <$> f (opts ^. headGap)
        <*> f (opts ^. tailGap)

-- | Same as gaps, provided for backward compatiiblity.
gap :: Traversal' (ArrowOpts n) (Measure n)
gap = gaps

-- | Style to apply to the head. @headStyle@ is modified by using the lens
--   combinator @%~@ to change the current style. For example, to change
--   an opaque black arrowhead to translucent orange:
--   @(with & headStyle %~ fc orange .  opacity 0.75)@.
headStyle :: Lens' (ArrowOpts n) (Style V2 n)

-- | Style to apply to the tail. See `headStyle`.
tailStyle :: Lens' (ArrowOpts n) (Style V2 n)

-- | Style to apply to the shaft. See `headStyle`.
shaftStyle :: Lens' (ArrowOpts n) (Style V2 n)

-- | The length from the start of the joint to the tip of the head.
headLength :: Lens' (ArrowOpts n) (Measure n)

-- | The length of the tail plus its joint.
tailLength :: Lens' (ArrowOpts n) (Measure n)

-- | Set both the @headLength@ and @tailLength@ simultaneously.
lengths :: Traversal' (ArrowOpts n) (Measure n)
lengths f opts = (\h t -> opts & headLength .~ h & tailLength .~ t) <$> f (opts ^. headLength)
             <*> f (opts ^. tailLength)

-- | A lens for setting or modifying the texture of an arrowhead. For
--   example, one may write @... (with & headTexture .~ grad)@ to get an
--   arrow with a head filled with a gradient, assuming grad has been
--   defined. Or @... (with & headTexture .~ solid blue@ to set the head
--   color to blue. For more general control over the style of arrowheads,
--   see 'headStyle'.
headTexture :: TypeableFloat n => Setter' (ArrowOpts n) (Texture n)
headTexture = headStyle . styleFillTexture

-- | A lens for setting or modifying the texture of an arrow
--   tail.
tailTexture :: TypeableFloat n => Setter' (ArrowOpts n) (Texture n)
tailTexture = tailStyle . styleFillTexture

-- | A lens for setting or modifying the texture of an arrow
--   shaft.
shaftTexture :: TypeableFloat n => Setter' (ArrowOpts n) (Texture n)
shaftTexture = shaftStyle . styleLineTexture

-- Set the default shaft style of an `ArrowOpts` record by applying the
-- default style after all other styles have been applied.
-- The semigroup stucture of the lw attribute will insure that the default
-- is only used if it has not been set in @opts@.
shaftSty :: Fractional n => ArrowOpts n -> Style V2 n
shaftSty opts = opts^.shaftStyle

-- Set the default head style. See `shaftSty`.
headSty :: TypeableFloat n => ArrowOpts n -> Style V2 n
headSty opts = fc black (opts^.headStyle)

-- Set the default tail style. See `shaftSty`.
tailSty :: TypeableFloat n => ArrowOpts n -> Style V2 n
tailSty opts = fc black (opts^.tailStyle)

-- | Calculate the length of the portion of the horizontal line that passes
--   through the origin and is inside of p.
xWidth :: Floating n => (Traced t, Vn t ~ V2 n) => t -> n
xWidth p = a + b
  where
    a = fromMaybe 0 (norm <$> traceV origin unitX p)
    b = fromMaybe 0 (norm <$> traceV origin unit_X p)

-- | Get the line color from the shaft to use as the fill color for the joint.
--   And set the opacity of the shaft to the current opacity.
colorJoint :: TypeableFloat n => Style V2 n -> Style V2 n
colorJoint sStyle =
  let c = fmap getLineTexture . getAttr $ sStyle
      o = fmap getOpacity . getAttr $ sStyle
  in
  case (c, o) of
      (Nothing, Nothing) -> fillColor (black :: Colour Double) mempty
      (Just t, Nothing)  -> fillTexture t mempty
      (Nothing, Just o') -> opacity o' . fillColor (black :: Colour Double) $ mempty
      (Just t, Just o')  -> opacity o' . fillTexture t $ mempty

-- | Get line width from a style.
widthOfJoint :: forall n. TypeableFloat n => Style V2 n -> n -> n -> n
widthOfJoint sStyle gToO nToO =
  maybe (fromMeasure gToO nToO medium) -- should be same as default line width
        (fromMeasure gToO nToO)
        (fmap getLineWidth . getAttr $ sStyle :: Maybe (Measure n))

-- | Combine the head and its joint into a single scale invariant diagram
--   and move the origin to the attachment point. Return the diagram
--   and its width.
mkHead :: (DataFloat n, Renderable (Path V2 n) b) =>
          n -> ArrowOpts n -> n -> n -> (Diagram b V2 n, n)
mkHead sz opts gToO nToO = ( (j <> h) # moveOriginBy (jWidth *^ unit_X) # lwO 0
                             , hWidth + jWidth)
  where
    (h', j') = (opts^.arrowHead) sz
               (widthOfJoint (shaftSty opts) gToO nToO)
    hWidth = xWidth h'
    jWidth = xWidth j'
    h = stroke h' # applyStyle (headSty opts)
    j = stroke j' # applyStyle (colorJoint (opts^.shaftStyle))

-- | Just like mkHead only the attachment point is on the right.
mkTail :: (DataFloat n, Renderable (Path V2 n) b) =>
          n -> ArrowOpts n -> n -> n -> (Diagram b V2 n, n)
mkTail sz opts gToO nToO = ((t <> j) # moveOriginBy (jWidth *^ unitX) # lwO 0
              , tWidth + jWidth)
  where
    (t', j') = (opts^.arrowTail) sz
               (widthOfJoint (shaftSty opts) gToO nToO)
    tWidth = xWidth t'
    jWidth = xWidth j'
    t = stroke t' # applyStyle (tailSty opts)
    j = stroke j' # applyStyle (colorJoint (opts^.shaftStyle))

-- | Make a trail with the same angles and offset as an arrow with tail width
--   tw, head width hw and shaft of tr, such that the magnituted of the shaft
--   offset is size. Used for calculating the offset of an arrow.
spine :: TypeableFloat n => Trail V2 n -> n -> n -> n -> Trail V2 n
spine tr tw hw sz = tS <> tr # scale sz <> hS
  where
    tSpine = trailFromOffsets [signorm . tangentAtStart $ tr] # scale tw
    hSpine = trailFromOffsets [signorm . tangentAtEnd $ tr] # scale hw
    hS = if hw > 0 then hSpine else mempty
    tS = if tw > 0 then tSpine else mempty

--  | Calculate the amount required to scale a shaft trail so that an arrow with
--    head width hw and tail width tw has offset t.
scaleFactor :: TypeableFloat n => Trail V2 n -> n -> n -> n -> n
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
             (quadrance v)
             (2* (v `dot` (tv ^+^ hv)))
             (quadrance (tv ^+^ hv) - t*t)
    of
      []  -> 1   -- no scale works, just return 1
      [s] -> s   -- single solution
      ss  -> maximum ss
        -- we will usually get both a positive and a negative solution;
        -- return the maximum (i.e. positive) solution
  where
    tv = tw *^ (tangentAtStart tr # signorm)
    hv = hw *^ (tangentAtEnd   tr # signorm)
    v  = trailOffset tr

-- Calculate the approximate envelope of a horizontal arrow
-- as if the arrow were made only of a shaft.
arrowEnv :: TypeableFloat n => ArrowOpts n -> n -> Envelope V2 n
arrowEnv opts len = getEnvelope horizShaft
  where
    horizShaft = shaft # rotate (negated (v ^. _theta)) # scale (len / m)
    m = norm v
    v = trailOffset shaft
    shaft = opts ^. arrowShaft

-- | @arrow len@ creates an arrow of length @len@ with default
--   parameters, starting at the origin and ending at the point
--   @(len,0)@.
arrow :: (DataFloat n, Renderable (Path V2 n) b) => n -> Diagram b V2 n
arrow = arrow' def

-- | @arrow' opts len@ creates an arrow of length @len@ using the
--   given options, starting at the origin and ending at the point
--   @(len,0)@.  In particular, it scales the given 'arrowShaft' so
--   that the entire arrow has length @len@.
arrow' :: (DataFloat n, Renderable (Path V2 n) b) => ArrowOpts n -> n -> Diagram b V2 n
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
        scaleFromMeasure = fromMeasure gToO nToO . scaleLocal (avgScale tr)
        hSize = scaleFromMeasure $ opts ^. headLength
        tSize = scaleFromMeasure $ opts ^. tailLength
        hGap  = scaleFromMeasure $ opts ^. headGap
        tGap  = scaleFromMeasure $ opts ^. tailGap

        -- Make the head and tail and save their widths.
        (h, hWidth') = mkHead hSize opts' gToO nToO
        (t, tWidth') = mkTail tSize opts' gToO nToO

        rawShaftTrail = opts^.arrowShaft
        shaftTrail
          = rawShaftTrail
            -- rotate it so it is pointing in the positive X direction
          # rotate (negated . view _theta . trailOffset $ rawShaftTrail)
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
        sf = scaleFactor shaftTrail tWidth hWidth (norm (q .-. p))
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
arrowBetween :: (DataFloat n, Renderable (Path V2 n) b) => Point V2 n -> Point V2 n -> Diagram b V2 n
arrowBetween = arrowBetween' def

-- | @arrowBetween' opts s e@ creates an arrow pointing from @s@ to
--   @e@ using the given options.  In particular, it scales and
--   rotates @arrowShaft@ to go between @s@ and @e@, taking head,
--   tail, and gaps into account.
arrowBetween'
  :: (DataFloat n, Renderable (Path V2 n) b) =>
     ArrowOpts n -> Point V2 n -> Point V2 n -> Diagram b V2 n
arrowBetween' opts s e = arrowAt' opts s (e .-. s)

-- | Create an arrow starting at s with length and direction determined by
--   the vector v.
arrowAt :: (DataFloat n, Renderable (Path V2 n) b) => Point V2 n -> V2 n -> Diagram b V2 n
arrowAt = arrowAt' def

arrowAt'
  :: (DataFloat n, Renderable (Path V2 n) b) =>
     ArrowOpts n -> Point V2 n -> V2 n -> Diagram b V2 n
arrowAt' opts s v = arrow' opts len
                  # rotate dir # moveTo s
  where
    len = norm v
    dir = v ^. _theta

-- | @arrowV v@ creates an arrow with the direction and norm of
--   the vector @v@ (with its tail at the origin), using default
--   parameters.
arrowV :: (DataFloat n, Renderable (Path V2 n) b) => V2 n -> Diagram b V2 n
arrowV = arrowV' def

-- | @arrowV' v@ creates an arrow with the direction and norm of
--   the vector @v@ (with its tail at the origin).
arrowV'
  :: (DataFloat n, Renderable (Path V2 n) b)
  => ArrowOpts n -> V2 n -> Diagram b V2 n
arrowV' opts = arrowAt' opts origin

-- | Turn a located trail into a default arrow by putting an 
--   arrowhead at the end of the trail.
arrowFromLocatedTrail 
  :: Renderable (Path R2) b 
  => Located (Trail R2) -> Diagram b R2
arrowFromLocatedTrail = arrowFromLocatedTrail' def

-- | Turn a located trail into an arrow using the given options.
arrowFromLocatedTrail' 
  :: Renderable (Path R2) b 
  => ArrowOpts -> Located (Trail R2) -> Diagram b R2
arrowFromLocatedTrail' opts trail = arrowBetween' opts' start end
  where 
    opts' = opts & arrowShaft .~ unLoc trail
    start = atStart trail
    end   = atEnd trail

-- | Connect two diagrams with a straight arrow.
connect
  :: (DataFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
  => n1 -> n2 -> Diagram b V2 n -> Diagram b V2 n
connect = connect' def

-- | Connect two diagrams with an arbitrary arrow.
connect'
  :: (DataFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
  => ArrowOpts n -> n1 -> n2 -> Diagram b V2 n -> Diagram b V2 n
connect' opts n1 n2 =
  withName n1 $ \sub1 ->
  withName n2 $ \sub2 ->
    let [s,e] = map location [sub1, sub2]
    in  atop (arrowBetween' opts s e)

-- | Connect two diagrams at point on the perimeter of the diagrams, choosen
--   by angle.
connectPerim
  :: (DataFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
 => n1 -> n2 -> Angle n -> Angle n
  -> Diagram b V2 n -> Diagram b V2 n
connectPerim = connectPerim' def

connectPerim'
  :: (DataFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
  => ArrowOpts n -> n1 -> n2 -> Angle n -> Angle n
  -> Diagram b V2 n -> Diagram b V2 n
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
  :: (DataFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
  => n1 -> n2 -> Diagram b V2 n -> Diagram b V2 n
connectOutside = connectOutside' def

connectOutside'
  :: (DataFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
  => ArrowOpts n -> n1 -> n2 -> Diagram b V2 n -> Diagram b V2 n
connectOutside' opts n1 n2 =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
    let v = location b2 .-. location b1
        midpoint = location b1 .+^ (v ^/ 2)
        s' = fromMaybe (location b1) $ traceP midpoint (negated v) b1
        e' = fromMaybe (location b2) $ traceP midpoint v b2
    in
      atop (arrowBetween' opts s' e')

