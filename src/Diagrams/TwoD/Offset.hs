{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Offset
-- Copyright   :  (c) 2012 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Compute offsets to segments in two dimensions.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Offset
    ( offsetSegment

    , OffsetOpts(..), offsetJoin, offsetMiterLimit, offsetEpsilon
    , offsetTrail
    , offsetTrail'
    , offsetPath
    , offsetPath'

    , ExpandOpts(..), expandJoin, expandMiterLimit, expandCap, expandEpsilon
    , expandTrail
    , expandTrail'
    , expandPath
    , expandPath'

    ) where

import Control.Applicative
import Control.Lens            hiding (at, moveTo)

import Data.AffineSpace
import Data.Monoid
import Data.Monoid.Inf
import Data.VectorSpace

import Data.Default.Class

import Diagrams.Core

import Diagrams.Attributes
import Diagrams.Located
import Diagrams.Parametric
import Diagrams.Path
import Diagrams.Segment
import Diagrams.Trail           hiding (offset, isLoop)
import Diagrams.TrailLike
import Diagrams.TwoD.Arc
import Diagrams.TwoD.Curvature
import Diagrams.TwoD.Path
import Diagrams.TwoD.Types
import Diagrams.TwoD.Vector     (perp, direction)

unitPerp :: R2 -> R2
unitPerp = normalized . perp

perpAtParam :: Segment Closed R2 -> Double -> R2
perpAtParam   (Linear (OffsetClosed a))  _ = -unitPerp a
perpAtParam s@(Cubic _ _ _)              t = -unitPerp a
  where
    (Cubic a _ _) = snd $ splitAtParam s t

-- | Compute the offset of a segment.  Given a segment compute the offset
--   curve that is a fixed distance from the original curve.  For linear
--   segments nothing special happens, the same linear segment is returned
--   with a point that is offset by a perpendicular vector of the given offset
--   length.
--
--   Cubic segments require a search for a subdivision of cubic segments that
--   gives an approximation of the offset within the given epsilon factor
--   (the given epsilon factor is applied to the radius giving a concrete epsilon
--   value).
--   We must do this because the offset of a cubic is not a cubic itself (the
--   degree of the curve increases).  Cubics do, however, approach constant
--   curvature as we subdivide.  In light of this we scale the handles of
--   the offset cubic segment in proportion to the radius of curvature difference
--   between the original subsegment and the offset which will have a radius
--   increased by the offset parameter.
--
--   In the following example the blue lines are the original segments and
--   the alternating green and red lines are the resulting offset trail segments.
--
--   <<diagrams/src_Diagrams_TwoD_Offset_cubicOffsetExample.svg#diagram=cubicOffsetExample&width=600>>
--
--   Note that when the original curve has a cusp, the offset curve forms a
--   radius around the cusp, and when there is a loop in the original curve,
--   there can be two cusps in the offset curve.
--

-- | Options for specifying line join and segment epsilon for an offset
--   involving multiple segments.
data OffsetOpts = OffsetOpts
    { _offsetJoin :: LineJoin
    , _offsetMiterLimit :: Double
    , _offsetEpsilon :: Double
    } deriving (Eq, Show)

makeLensesWith (lensRules & generateSignatures .~ False) ''OffsetOpts

-- | Specifies the style of join for between adjacent offset segments.
offsetJoin :: Lens' OffsetOpts LineJoin

-- | Specifies the miter limit for the join.
offsetMiterLimit :: Lens' OffsetOpts Double

-- | Epsilon perimeter for 'offsetSegment'.
offsetEpsilon :: Lens' OffsetOpts Double

-- | The default offset options use the default 'LineJoin' ('LineJoinMiter'), a
--   miter limit of 10, and epsilon factor of 0.01.
instance Default OffsetOpts where
    def = OffsetOpts def 10 0.01

-- | Options for specifying how a 'Trail' should be expanded.
data ExpandOpts = ExpandOpts
    { _expandJoin :: LineJoin
    , _expandMiterLimit :: Double
    , _expandCap  :: LineCap
    , _expandEpsilon :: Double
    } deriving (Eq, Show)

makeLensesWith (lensRules & generateSignatures .~ False) ''ExpandOpts

-- | Specifies the style of join for between adjacent offset segments.
expandJoin :: Lens' ExpandOpts LineJoin

-- | Specifies the miter limit for the join.
expandMiterLimit :: Lens' ExpandOpts Double

-- | Specifies how the ends are handled.
expandCap :: Lens' ExpandOpts LineCap

-- | Epsilon perimeter for 'offsetSegment'.
expandEpsilon :: Lens' ExpandOpts Double



-- | The default 'ExpandOpts' is the default 'LineJoin' ('LineJoinMiter'),
--   miter limit of 10, default 'LineCap' ('LineCapButt'), and epsilon factor
--   of 0.01.
instance Default ExpandOpts where
    def = ExpandOpts def 10 def 0.01

offsetSegment :: Double     -- ^ Epsilon factor that when multiplied to the
                            --   absolute value of the radius gives a
                            --   value that represents the maximum
                            --   allowed deviation from the true offset.  In
                            --   the current implementation each result segment
                            --   should be bounded by arcs that are plus or
                            --   minus epsilon factor from the radius of curvature of
                            --   the offset.
              -> Double     -- ^ Offset from the original segment, positive is
                            --   on the right of the curve, negative is on the
                            --   left.
              -> Segment Closed R2  -- ^ Original segment
              -> Located (Trail R2) -- ^ Resulting located (at the offset) trail.
offsetSegment _       r s@(Linear (OffsetClosed a))    = trailFromSegments [s] `at` origin .+^ va
  where va = -r *^ unitPerp a

offsetSegment epsilon r s@(Cubic a b (OffsetClosed c)) = t `at` origin .+^ va
  where
    t = trailFromSegments (go (radiusOfCurvature s 0.5))
    -- Perpendiculars to handles.
    va = -r *^ unitPerp a
    vc = -r *^ unitPerp (c ^-^ b)
    -- Split segments.
    ss = (\(x,y) -> [x,y]) $ splitAtParam s 0.5
    subdivided = concatMap (trailSegments . unLoc . offsetSegment epsilon r) ss

    -- Offset with handles scaled based on curvature.
    offset factor = bezier3 (a^*factor) ((b ^-^ c)^*factor ^+^ c ^+^ vc ^-^ va) (c ^+^ vc ^-^ va)

    -- We observe a corner.  Subdivide right away.
    go (Finite 0) = subdivided
    -- We have some curvature
    go roc
      | close     = [o]
      | otherwise = subdivided
      where
        -- We want the multiplicative factor that takes us from the original
        -- segment's radius of curvature roc, to roc + r.
        --
        -- r + sr = x * sr
        --
        o = offset $ case roc of
              Infinity  -> 1          -- Do the right thing.
              Finite sr -> 1 + r / sr

        close = and [epsilon * abs r > (magnitude (p o ^+^ va ^-^ p s ^-^ pp s))
                    | t' <- [0.25, 0.5, 0.75]
                    , let p = (`atParam` t')
                    , let pp = (r *^) . (`perpAtParam` t')
                    ]


-- > import Diagrams.TwoD.Offset
-- > import Diagrams.Coordinates
-- >
-- > showExample :: Segment Closed R2 -> Diagram SVG R2
-- > showExample s = pad 1.1 . centerXY $ d # lc blue # lw 0.1 <> d' # lw 0.1
-- >   where
-- >       d  = stroke . fromSegments $ [s]
-- >       d' = mconcat . zipWith lc colors . map stroke . explodeTrail
-- >          $ offsetSegment 0.1 (-1) s
-- >
-- >       colors = cycle [green, red]
-- >
-- > cubicOffsetExample :: Diagram SVG R2
-- > cubicOffsetExample = hcat . map showExample $
-- >         [ bezier3 (10 &  0) (  5  & 18) (10 & 20)
-- >         , bezier3 ( 0 & 20) ( 10  & 10) ( 5 & 10)
-- >         , bezier3 (10 & 20) (  0  & 10) (10 &  0)
-- >         , bezier3 (10 & 20) ((-5) & 10) (10 &  0)
-- >         ]

-- Similar to (=<<).  This is when we want to map a function across something
-- located, but the result of the mapping will be transformable so we can
-- collapse the Located into the result.  This assumes that Located has the
-- meaning of merely taking something that cannot be translated and lifting
-- it into a space with translation.
bindLoc :: (Transformable b, V a ~ V b) => (a -> b) -> Located a -> b
bindLoc f = join' . mapLoc f
  where
    join' (viewLoc -> (p,a)) = translate (p .-. origin) a

-- While we build offsets and expansions we will use the [Located (Segment Closed R2)]
-- and [Located (Trail R2)] intermediate representations.
locatedTrailSegments :: (InnerSpace v, OrderedField (Scalar v))
                     => Located (Trail v) -> [Located (Segment Closed v)]
locatedTrailSegments t = zipWith at (trailSegments (unLoc t)) (trailVertices t)

-- | Offset a 'Trail' with options and by a given radius.  This generates a new
--   trail that is always radius 'r' away from the given 'Trail' (depending on
--   the line join option) on the right.
--
--   The styles applied to an outside corner can be seen here (with the original
--   trail in blue and the result of 'offsetTrail'' in green):
--
--   <<diagrams/src_Diagrams_TwoD_Offset_offsetTrailExample.svg#diagram=offsetTrailExample&width=600>>
--
--   When a negative radius is given, the offset trail will be on the left:
--
--   <<diagrams/src_Diagrams_TwoD_Offset_offsetTrailLeftExample.svg#diagram=offsetTrailLeftExample&width=200>>
--
--   When offseting a counter-clockwise loop a positive radius gives an outer loop
--   while a negative radius gives an inner loop (both counter-clockwise).
--
--   <<diagrams/src_Diagrams_TwoD_Offset_offsetTrailOuterExample.svg#diagram=offsetTrailOuterExample&width=300>>
--
offsetTrail' :: OffsetOpts
             -> Double  -- ^ Radius of offset.  A negative value gives an offset on
                        --   the left for a line and on the inside for a counter-clockwise
                        --   loop.
             -> Located (Trail R2)
             -> Located (Trail R2)
offsetTrail' opts r t = joinSegments j isLoop (opts^.offsetMiterLimit) r ends . offset $ t
    where
      offset = map (bindLoc (offsetSegment (opts^.offsetEpsilon) r)) . locatedTrailSegments
      ends | isLoop    = (\(a:as) -> as ++ [a]) . trailVertices $ t
           | otherwise = tail . trailVertices $ t
      j = fromLineJoin (opts^.offsetJoin)

      isLoop = withTrail (const False) (const True) (unLoc t)

-- | Offset a 'Trail' with the default options and a given radius.  See 'offsetTrail''.
offsetTrail :: Double -> Located (Trail R2) -> Located (Trail R2)
offsetTrail = offsetTrail' def

-- | Offset a 'Path' by applying 'offsetTrail'' to each trail in the path.
offsetPath' :: OffsetOpts -> Double -> Path R2 -> Path R2
offsetPath' opts r = mconcat
                   . map (bindLoc (trailLike . offsetTrail' opts r) . (`at` origin))
                   . view pathTrails

-- | Offset a 'Path' with the default options and given radius.  See 'offsetPath''.
offsetPath :: Double -> Path R2 -> Path R2
offsetPath = offsetPath' def

-- TODO: Include arrowheads on examples to indicate direction so the "left" and
-- "right" make sense.
--
-- > import Diagrams.TwoD.Offset
-- > import Data.Default.Class
-- > import Control.Lens ((&), (.~), (%~))
-- >
-- > corner :: Located (Trail R2)
-- > corner = fromVertices (map p2 [(0, 0), (10, 0), (5, 6)]) `at` origin
-- >
-- > offsetTrailExample :: Diagram SVG R2
-- > offsetTrailExample = pad 1.1 . centerXY . lw 0.2 . hcat' (def & sep .~ 1 )
-- >                    . map (uncurry showStyle)
-- >                    $ [ (LineJoinMiter, "LineJoinMiter")
-- >                      , (LineJoinRound, "LineJoinRound")
-- >                      , (LineJoinBevel, "LineJoinBevel")
-- >                      ]
-- >  where
-- >    showStyle j s = centerXY (trailLike corner # lc blue
-- >               <> trailLike (offsetTrail' (def & offsetJoin .~ j) 2 corner) # lc green)
-- >            === (strutY 3 <> text s # font "Helvetica" # bold)
-- >
-- > offsetTrailLeftExample :: Diagram SVG R2
-- > offsetTrailLeftExample = pad 1.1 . centerXY . lw 0.2
-- >                        $ (trailLike c # lc blue)
-- >                        <> (lc green . trailLike
-- >                         . offsetTrail' (def & offsetJoin .~ LineJoinRound) (-2) $ c)
-- >   where
-- >     c = reflectY corner
-- >
-- > offsetTrailOuterExample :: Diagram SVG R2
-- > offsetTrailOuterExample = pad 1.1 . centerXY . lw 0.2
-- >                         $ (trailLike c # lc blue)
-- >                         <> (lc green . trailLike
-- >                          . offsetTrail' (def & offsetJoin .~ LineJoinRound) 2 $ c)
-- >   where
-- >     c = hexagon 5

withTrailL :: (Located (Trail' Line v) -> r) -> (Located (Trail' Loop v) -> r) -> Located (Trail v) -> r
withTrailL f g l = withTrail (f . (`at` p)) (g . (`at` p)) (unLoc l)
  where
    p = loc l

-- | Expand a 'Trail' with the given options and radius 'r' around a given 'Trail'.
--   Expanding can be thought of as generating the loop that, when filled, represents
--   stroking the trail with a radius 'r' brush.
--
--   The cap styles applied to an outside corner can be seen here (with the original
--   trail in white and the result of 'expandTrail'' filled in green):
--
--   <<diagrams/src_Diagrams_TwoD_Offset_expandTrailExample.svg#diagram=expandTrailExample&width=600>>
--
--   Loops result in a path with an inner and outer loop:
--
--   <<diagrams/src_Diagrams_TwoD_Offset_expandLoopExample.svg#diagram=expandLoopExample&width=300>>
--
expandTrail' :: ExpandOpts
             -> Double  -- ^ Radius of offset.  Only non-negative values allowed.
                        --   For a line this gives a loop of the offset.  For a
                        --   loop this gives two loops, the outer counter-clockwise
                        --   and the inner clockwise.
             -> Located (Trail R2)
             -> Path R2
expandTrail' o r t
  | r < 0     = error "expandTrail' with negative radius"
                -- TODO: consider just reversing the path instead of this error.
  | otherwise = withTrailL (pathFromLocTrail . expandLine o r) (expandLoop o r) t

expandLine :: ExpandOpts -> Double -> Located (Trail' Line R2) -> Located (Trail R2)
expandLine opts r (mapLoc wrapLine -> t) = caps cap r s e (f r) (f $ -r)
    where
      offset r' = map (bindLoc (offsetSegment (opts^.expandEpsilon) r')) . locatedTrailSegments
      f r' = joinSegments (fromLineJoin (opts^.expandJoin)) False (opts^.expandMiterLimit) r' ends . offset r' $ t
      ends = tail . trailVertices $ t
      s = atStart t
      e = atEnd t
      cap = fromLineCap (opts^.expandCap)

expandLoop :: ExpandOpts -> Double -> Located (Trail' Loop R2) -> Path R2
expandLoop opts r (mapLoc wrapLoop -> t) = (trailLike $ f r) <> (trailLike . reverseDomain . f $ -r)
    where
      offset r' = map (bindLoc (offsetSegment (opts^.expandEpsilon) r')) . locatedTrailSegments
      f r' = joinSegments (fromLineJoin (opts^.expandJoin)) True (opts^.expandMiterLimit) r' ends . offset r' $ t
      ends = (\(a:as) -> as ++ [a]) . trailVertices $ t

-- | Expand a 'Trail' with the given radius and default options.  See 'expandTrail''.
expandTrail :: Double -> Located (Trail R2) -> Path R2
expandTrail = expandTrail' def

-- | Expand a 'Path' using 'expandTrail'' on each trail in the path.
expandPath' :: ExpandOpts -> Double -> Path R2 -> Path R2
expandPath' opts r = mconcat
                   . map (bindLoc (expandTrail' opts r) . (`at` origin))
                   . view pathTrails

-- | Expand a 'Path' with the given radius and default options.  See 'expandPath''.
expandPath :: Double -> Path R2 -> Path R2
expandPath = expandPath' def

-- > import Diagrams.TwoD.Offset
-- > import Data.Default.Class
-- > import Control.Lens ((&), (.~), (%~))
-- >
-- > expandTrailExample :: Diagram SVG R2
-- > expandTrailExample = pad 1.1 . centerXY . hcat' (def & sep .~ 1)
-- >                    . map (uncurry showStyle)
-- >                    $ [ (LineCapButt,   "LineCapButt")
-- >                      , (LineCapRound,  "LineCapRound")
-- >                      , (LineCapSquare, "LineCapSquare")
-- >                      ]
-- >  where
-- >    showStyle c s = centerXY (trailLike corner # lc white # lw 0.2
-- >                               <> stroke (expandTrail'
-- >                                              (def & expandJoin .~ LineJoinRound
-- >                                                   & expandCap .~ c
-- >                                                   ) 2 corner)
-- >                                      # lw 0 # fc green)
-- >               === (strutY 3 <> text s # font "Helvetica" # bold)
-- >
-- > expandLoopExample :: Diagram SVG R2
-- > expandLoopExample = pad 1.1 . centerXY $ ((strokeLocT t # lw 0.2 # lc white)
-- >                                        <> (stroke t' # lw 0 # fc green))
-- >   where
-- >     t  = mapLoc glueTrail $ fromVertices (map p2 [(0, 0), (5, 0), (10, 5), (10, 10), (0, 0)])
-- >     t' = expandTrail' (def & expandJoin .~ LineJoinRound) 1 t


-- | When we expand a line (the original line runs through the center of offset
--   lines at  r  and  -r) there is some choice in what the ends will look like.
--   If we are using a circle brush we should see a half circle at each end.
--   Similar caps could be made for square brushes or simply stopping exactly at
--   the end with a straight line (a perpendicular line brush).
--
--   caps  takes the radius and the start and end points of the original line and
--   the offset trails going out and coming back.  The result is a new list of
--   trails with the caps included.
caps :: (Double -> P2 -> P2 -> P2 -> Trail R2)
     -> Double -> P2 -> P2 -> Located (Trail R2) -> Located (Trail R2) -> Located (Trail R2)
caps cap r s e fs bs = mapLoc glueTrail $ mconcat
    [ cap r s (atStart bs) (atStart fs)
    , unLoc fs
    , cap r e (atEnd fs) (atEnd bs)
    , reverseDomain (unLoc bs)
    ] `at` atStart bs

-- | Take a LineCap style and give a function for building the cap from
fromLineCap :: LineCap -> Double -> P2 -> P2 -> P2 -> Trail R2
fromLineCap c = case c of
    LineCapButt   -> capCut
    LineCapRound  -> capArc
    LineCapSquare -> capSquare

-- | Builds a cap that directly connects the ends.
capCut :: Double -> P2 -> P2 -> P2 -> Trail R2
capCut _r _c a b = fromSegments [straight (b .-. a)]

-- | Builds a cap with a square centered on the end.
capSquare :: Double -> P2 -> P2 -> P2 -> Trail R2
capSquare _r c a b = unLoc $ fromVertices [ a, a .+^ v, b .+^ v, b ]
  where
    v = perp (a .-. c)

-- | Builds an arc to fit with a given radius, center, start, and end points.
--   A Negative r means a counter-clockwise arc
capArc :: Double -> P2 -> P2 -> P2 -> Trail R2
capArc r c a b = trailLike . moveTo c $ fs
  where
    fs | r < 0     = scale (-r) $ arcVCW (a .-. c) (b .-. c)
       | otherwise = scale r    $ arcV   (a .-. c) (b .-. c)

-- Arc helpers
arcV :: (TrailLike t, V t ~ R2) => R2 -> R2 -> t
arcV u v = arc (direction u) (direction v :: Turn)

arcVCW :: (TrailLike t, V t ~ R2) => R2 -> R2 -> t
arcVCW u v = arcCW (direction u) (direction v :: Turn)


-- | Join together a list of located trails with the given join style.  The
--   style is given as a function to compute the join given the local information
--   of the original vertex, the previous trail, and the next trail.  The result
--   is a single located trail.  A join radius is also given to aid in arc joins.
--
--   Note: this is not a general purpose join and assumes that we are joining an
--   offset trail.  For instance, a fixed radius arc will not fit between arbitrary
--   trails without trimming or extending.
joinSegments :: (Double -> Double -> P2 -> Located (Trail R2) -> Located (Trail R2) -> Trail R2)
             -> Bool
             -> Double
             -> Double
             -> [Point R2]
             -> [Located (Trail R2)]
             -> Located (Trail R2)
joinSegments _ _ _ _ _ [] = mempty `at` origin
joinSegments _ _ _ _ [] _ = mempty `at` origin
joinSegments j isLoop ml r es ts@(t:_) = t'
  where
    t' | isLoop    = mapLoc (glueTrail . (<> mconcat (take (length ts) $ ss es (ts ++ [t])))) t
       | otherwise = mapLoc (<> mconcat (ss es ts)) t
    ss es' ts' = [j ml r e a b <> unLoc b | (e,(a,b)) <- zip es' . (zip <*> tail) $ ts']

-- | Take a join style and give the join function to be used by joinSegments.
fromLineJoin
  :: LineJoin -> Double -> Double -> P2 -> Located (Trail R2) -> Located (Trail R2) -> Trail R2
fromLineJoin j = case j of
    LineJoinMiter -> joinSegmentIntersect
    LineJoinRound -> joinSegmentArc
    LineJoinBevel -> joinSegmentClip

-- TODO: The joinSegmentCut option is not in our standard line joins.  I don't know
-- how useful it is graphically, I mostly had it as it was useful for debugging
{-
-- | Join with segments going back to the original corner.
joinSegmentCut :: Double -> Double -> P2 -> Located (Trail R2) -> Located (Trail R2) -> Trail R2
joinSegmentCut _ _ e a b = fromSegments
    [ straight (e .-. atEnd a)
    , straight (atStart b .-. e)
    ]
-}

-- | Join by directly connecting the end points.  On an inside corner this
--   creates negative space for even-odd fill.  Here is where we would want to
--   use an arc or something else in the future.
joinSegmentClip :: Double -> Double -> P2 -> Located (Trail R2) -> Located (Trail R2) -> Trail R2
joinSegmentClip _ _ _ a b = fromSegments [straight $ atStart b .-. atEnd a]

-- | Join with a radius arc.  On an inside corner this will loop around the interior
--   of the offset trail.  With a winding fill this will not be visible.
joinSegmentArc :: Double -> Double -> P2 -> Located (Trail R2) -> Located (Trail R2) -> Trail R2
joinSegmentArc _ r e a b = capArc r e (atEnd a) (atStart b)

-- | Join to the intersection of the incoming trails projected tangent to their ends.
--   If the intersection is beyond the miter limit times the radius, stop at the limit.
joinSegmentIntersect
    :: Double -> Double -> P2 -> Located (Trail R2) -> Located (Trail R2) -> Trail R2
joinSegmentIntersect miterLimit r e a b =
    case traceP pa va t of
      -- clip join when we excede the miter limit.  We could instead
      -- Join at exactly the miter limit, but standard behavior seems
      -- to be clipping.
      Nothing -> joinSegmentClip miterLimit r e a b
      Just p
        -- If trace gave us garbage...
        | p `distance` pb > abs (miterLimit * r) -> joinSegmentClip miterLimit r e a b
        | otherwise                              -> unLoc $ fromVertices [ pa, p, pb ]
  where
    -- TODO: is there really no instance for Traced (Located (Trail R2)) ?
    t = strokeLocT (fromSegments [straight (miter vb)] `at` pb) :: Diagram NullBackend R2
    va = unitPerp (pa .-. e)
    vb = -unitPerp (pb .-. e)
    pa = atEnd a
    pb = atStart b
    miter v = (abs (miterLimit * r)) *^ v
