{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Offset
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Compute offsets to segments in two dimensions.  More details can be
-- found in the manual at
-- <http://projects.haskell.org/diagrams/doc/manual.html#offsets-of-segments-trails-and-paths>.
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Offset
    (
      -- * Offsets

      offsetSegment

    , OffsetOpts(..), offsetJoin, offsetMiterLimit, offsetEpsilon
    , offsetTrail
    , offsetTrail'
    , offsetPath
    , offsetPath'

      -- * Expansions

    , ExpandOpts(..), expandJoin, expandMiterLimit, expandCap, expandEpsilon
    , expandTrail
    , expandTrail'
    , expandPath
    , expandPath'

    ) where

import           Control.Applicative
import           Control.Lens            hiding (at)

import           Data.Maybe              (catMaybes)
import           Data.Monoid
import           Data.Monoid.Inf

import           Data.Default.Class

import           Diagrams.Core

import           Diagrams.Attributes
import           Diagrams.Direction
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Trail          hiding (isLoop, offset)
import           Diagrams.TrailLike
import           Diagrams.TwoD.Arc
import           Diagrams.TwoD.Curvature
import           Diagrams.TwoD.Path      ()
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector    hiding (e)

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

unitPerp :: OrderedField n => V2 n -> V2 n
unitPerp = signorm . perp

perpAtParam :: OrderedField n => Segment Closed V2 n -> n -> V2 n
perpAtParam (Linear (OffsetClosed a)) _ = negated $ unitPerp a
perpAtParam cubic t                     = negated $ unitPerp a
  where
    (Cubic a _ _) = snd $ splitAtParam cubic t

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
data OffsetOpts d = OffsetOpts
    { _offsetJoin       :: LineJoin
    , _offsetMiterLimit :: d
    , _offsetEpsilon    :: d
    }

deriving instance Eq d => Eq (OffsetOpts d)
deriving instance Show d => Show (OffsetOpts d)

makeLensesWith (lensRules & generateSignatures .~ False) ''OffsetOpts

-- | Specifies the style of join for between adjacent offset segments.
offsetJoin :: Lens' (OffsetOpts d) LineJoin

-- | Specifies the miter limit for the join.
offsetMiterLimit :: Lens' (OffsetOpts d) d

-- | Epsilon perimeter for 'offsetSegment'.
offsetEpsilon :: Lens' (OffsetOpts d) d

-- | The default offset options use the default 'LineJoin' ('LineJoinMiter'), a
--   miter limit of 10, and epsilon factor of 0.01.
instance Fractional d => Default (OffsetOpts d) where
    def = OffsetOpts def 10 0.01

-- | Options for specifying how a 'Trail' should be expanded.
data ExpandOpts d = ExpandOpts
    { _expandJoin       :: LineJoin
    , _expandMiterLimit :: d
    , _expandCap        :: LineCap
    , _expandEpsilon    :: d
    } deriving (Eq, Show)

makeLensesWith (lensRules & generateSignatures .~ False) ''ExpandOpts

-- | Specifies the style of join for between adjacent offset segments.
expandJoin :: Lens' (ExpandOpts d) LineJoin

-- | Specifies the miter limit for the join.
expandMiterLimit :: Lens' (ExpandOpts d) d

-- | Specifies how the ends are handled.
expandCap :: Lens' (ExpandOpts d) LineCap

-- | Epsilon perimeter for 'offsetSegment'.
expandEpsilon :: Lens' (ExpandOpts d) d



-- | The default 'ExpandOpts' is the default 'LineJoin' ('LineJoinMiter'),
--   miter limit of 10, default 'LineCap' ('LineCapButt'), and epsilon factor
--   of 0.01.
instance (Fractional d) => Default (ExpandOpts d) where
    def = ExpandOpts def 10 def 0.01

offsetSegment :: RealFloat n
              => n   -- ^ Epsilon factor that when multiplied to the
                            --   absolute value of the radius gives a
                            --   value that represents the maximum
                            --   allowed deviation from the true offset.  In
                            --   the current implementation each result segment
                            --   should be bounded by arcs that are plus or
                            --   minus epsilon factor from the radius of curvature of
                            --   the offset.
              -> n   -- ^ Offset from the original segment, positive is
                            --   on the right of the curve, negative is on the
                            --   left.
              -> Segment Closed V2 n  -- ^ Original segment
              -> Located (Trail V2 n) -- ^ Resulting located (at the offset) trail.
offsetSegment _       r s@(Linear (OffsetClosed a))    = trailFromSegments [s] `at` origin .+^ va
  where va = (-r) *^ unitPerp a

offsetSegment epsilon r s@(Cubic a b (OffsetClosed c)) = t `at` origin .+^ va
  where
    t = trailFromSegments (go (radiusOfCurvature s 0.5))
    -- Perpendiculars to handles.
    va = (-r) *^ unitPerp a
    vc = (-r) *^ unitPerp (c ^-^ b)
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

        close = and [epsilon * abs r > norm (p o ^+^ va ^-^ p s ^-^ pp s)
                    | t' <- [0.25, 0.5, 0.75]
                    , let p = (`atParam` t')
                    , let pp = (r *^) . (`perpAtParam` t')
                    ]


-- > import Diagrams.TwoD.Offset
-- >
-- > showExample :: (OrderedField n) => Segment Closed v -> Diagram SVG v
-- > showExample s = pad 1.1 . centerXY $ d # lc blue # lw thick <> d' # lw thick
-- >   where
-- >       d  = stroke . fromSegments $ [s]
-- >       d' = mconcat . zipWith lc colors . map stroke . explodeTrail
-- >          $ offsetSegment 0.1 (-1) s
-- >
-- >       colors = cycle [green, red]
-- >
-- > cubicOffsetExample :: (OrderedField n) => Diagram SVG v
-- > cubicOffsetExample = hcat . map showExample $
-- >         [ bezier3 (10 ^&  0) (  5  ^& 18) (10 ^& 20)
-- >         , bezier3 ( 0 ^& 20) ( 10  ^& 10) ( 5 ^& 10)
-- >         , bezier3 (10 ^& 20) (  0  ^& 10) (10 ^&  0)
-- >         , bezier3 (10 ^& 20) ((-5) ^& 10) (10 ^&  0)
-- >         ]

-- Similar to (=<<).  This is when we want to map a function across something
-- located, but the result of the mapping will be transformable so we can
-- collapse the Located into the result.  This assumes that Located has the
-- meaning of merely taking something that cannot be translated and lifting
-- it into a space with translation.
bindLoc :: (Transformable b, V a ~ V b, N a ~ N b, V a ~ V2, N a ~ n, Num n) => (a -> b) -> Located a -> b
bindLoc f = join' . mapLoc f
  where
    join' (viewLoc -> (p,a)) = translate (p .-. origin) a

-- While we build offsets and expansions we will use the [Located (Segment Closed v)]
-- and [Located (Trail V2 n)] intermediate representations.
locatedTrailSegments :: OrderedField n
                     => Located (Trail V2 n) -> [Located (Segment Closed V2 n)]
locatedTrailSegments t = zipWith at (trailSegments (unLoc t)) (trailPoints t)

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
offsetTrail' :: RealFloat n
             => OffsetOpts n
             -> n -- ^ Radius of offset.  A negative value gives an offset on
                         --   the left for a line and on the inside for a counter-clockwise
                         --   loop.
             -> Located (Trail V2 n)
             -> Located (Trail V2 n)
offsetTrail' opts r t = joinSegments eps j isLoop (opts^.offsetMiterLimit) r ends . offset $ t
    where
      eps = opts^.offsetEpsilon
      offset = map (bindLoc (offsetSegment eps r)) . locatedTrailSegments
      ends | isLoop    = (\(a:as) -> as ++ [a]) . trailVertices $ t
           | otherwise = tail . trailVertices $ t
      j = fromLineJoin (opts^.offsetJoin)

      isLoop = withTrail (const False) (const True) (unLoc t)

-- | Offset a 'Trail' with the default options and a given radius.  See 'offsetTrail''.
offsetTrail :: RealFloat n => n -> Located (Trail V2 n) -> Located (Trail V2 n)
offsetTrail = offsetTrail' def

-- | Offset a 'Path' by applying 'offsetTrail'' to each trail in the path.
offsetPath' :: RealFloat n => OffsetOpts n -> n -> Path V2 n -> Path V2 n
offsetPath' opts r = mconcat
                   . map (bindLoc (trailLike . offsetTrail' opts r) . (`at` origin))
                   . op Path

-- | Offset a 'Path' with the default options and given radius.  See 'offsetPath''.
offsetPath :: RealFloat n => n -> Path V2 n -> Path V2 n
offsetPath = offsetPath' def

-- TODO: Include arrowheads on examples to indicate direction so the "left" and
-- "right" make sense.
--
-- > import Diagrams.TwoD.Offset
-- > import Data.Default.Class
-- >
-- > corner :: (OrderedField n) => Located (Trail V2 n)
-- > corner = fromVertices (map p2 [(0, 0), (10, 0), (5, 6)]) `at` origin
-- >
-- > offsetTrailExample :: (OrderedField n) => Diagram SVG v
-- > offsetTrailExample = pad 1.1 . centerXY . lwO 3 . hcat' (def & sep .~ 1 )
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
-- > offsetTrailLeftExample :: (OrderedField n) => Diagram SVG v
-- > offsetTrailLeftExample = pad 1.1 . centerXY . lwO 3
-- >                        $ (trailLike c # lc blue)
-- >                        <> (lc green . trailLike
-- >                         . offsetTrail' (def & offsetJoin .~ LineJoinRound) (-2) $ c)
-- >   where
-- >     c = reflectY corner
-- >
-- > offsetTrailOuterExample :: (OrderedField n) => Diagram SVG v
-- > offsetTrailOuterExample = pad 1.1 . centerXY . lwO 3
-- >                         $ (trailLike c # lc blue)
-- >                         <> (lc green . trailLike
-- >                          . offsetTrail' (def & offsetJoin .~ LineJoinRound) 2 $ c)
-- >   where
-- >     c = hexagon 5

withTrailL :: (OrderedField n) => (Located (Trail' Line V2 n) -> r) -> (Located (Trail' Loop V2 n) -> r) -> Located (Trail V2 n) -> r
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
expandTrail' :: (OrderedField n, RealFloat n, RealFrac n)
             => ExpandOpts n
             -> n  -- ^ Radius of offset.  Only non-negative values allowed.
                        --   For a line this gives a loop of the offset.  For a
                        --   loop this gives two loops, the outer counter-clockwise
                        --   and the inner clockwise.
             -> Located (Trail V2 n)
             -> Path V2 n
expandTrail' o r t
  | r < 0     = error "expandTrail' with negative radius"
                -- TODO: consider just reversing the path instead of this error.
  | otherwise = withTrailL (pathFromLocTrail . expandLine o r) (expandLoop o r) t

expandLine :: RealFloat n => ExpandOpts n -> n -> Located (Trail' Line V2 n) -> Located (Trail V2 n)
expandLine opts r (mapLoc wrapLine -> t) = caps cap r s e (f r) (f $ -r)
    where
      eps = opts^.expandEpsilon
      offset r' = map (bindLoc (offsetSegment eps r')) . locatedTrailSegments
      f r' = joinSegments eps (fromLineJoin (opts^.expandJoin)) False (opts^.expandMiterLimit) r' ends
           . offset r' $ t
      ends = tail . trailVertices $ t
      s = atStart t
      e = atEnd t
      cap = fromLineCap (opts^.expandCap)

expandLoop :: RealFloat n => ExpandOpts n -> n -> Located (Trail' Loop V2 n) -> Path V2 n
expandLoop opts r (mapLoc wrapLoop -> t) = trailLike (f r) <> (trailLike . reverseDomain . f $ -r)
    where
      eps = opts^.expandEpsilon
      offset r' = map (bindLoc (offsetSegment eps r')) . locatedTrailSegments
      f r' = joinSegments eps (fromLineJoin (opts^.expandJoin)) True (opts^.expandMiterLimit) r' ends
           . offset r' $ t
      ends = (\(a:as) -> as ++ [a]) . trailVertices $ t

-- | Expand a 'Trail' with the given radius and default options.  See 'expandTrail''.
expandTrail :: RealFloat n => n -> Located (Trail V2 n) -> Path V2 n
expandTrail = expandTrail' def

-- | Expand a 'Path' using 'expandTrail'' on each trail in the path.
expandPath' :: RealFloat n => ExpandOpts n -> n -> Path V2 n -> Path V2 n
expandPath' opts r = mconcat
                   . map (bindLoc (expandTrail' opts r) . (`at` origin))
                   . op Path

-- | Expand a 'Path' with the given radius and default options.  See 'expandPath''.
expandPath :: RealFloat n => n -> Path V2 n -> Path V2 n
expandPath = expandPath' def

-- > import Diagrams.TwoD.Offset
-- > import Data.Default.Class
-- >
-- > expandTrailExample :: (OrderedField n) => Diagram SVG v
-- > expandTrailExample = pad 1.1 . centerXY . hcat' (def & sep .~ 1)
-- >                    . map (uncurry showStyle)
-- >                    $ [ (LineCapButt,   "LineCapButt")
-- >                      , (LineCapRound,  "LineCapRound")
-- >                      , (LineCapSquare, "LineCapSquare")
-- >                      ]
-- >  where
-- >    showStyle c s = centerXY (trailLike corner # lc white # lw veryThick
-- >                               <> stroke (expandTrail'
-- >                                              (def & expandJoin .~ LineJoinRound
-- >                                                   & expandCap .~ c
-- >                                                   ) 2 corner)
-- >                                      # lw none # fc green)
-- >               === (strutY 3 <> text s # font "Helvetica" # bold)
-- >
-- > expandLoopExample :: (OrderedField n) => Diagram SVG v
-- > expandLoopExample = pad 1.1 . centerXY $ ((strokeLocT t # lw veryThick # lc white)
-- >                                        <> (stroke t' # lw none # fc green))
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
caps :: RealFloat n => (n -> Point V2 n -> Point V2 n -> Point V2 n -> Trail V2 n)
     -> n -> Point V2 n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Located (Trail V2 n)
caps cap r s e fs bs = mapLoc glueTrail $ mconcat
    [ cap r s (atStart bs) (atStart fs)
    , unLoc fs
    , cap r e (atEnd fs) (atEnd bs)
    , reverseDomain (unLoc bs)
    ] `at` atStart bs

-- | Take a LineCap style and give a function for building the cap from
fromLineCap :: RealFloat n => LineCap -> n -> Point V2 n -> Point V2 n -> Point V2 n -> Trail V2 n
fromLineCap c = case c of
    LineCapButt   -> capCut
    LineCapRound  -> capArc
    LineCapSquare -> capSquare

-- | Builds a cap that directly connects the ends.
capCut :: RealFloat n => n -> Point V2 n -> Point V2 n -> Point V2 n -> Trail V2 n
capCut _r _c a b = fromSegments [straight (b .-. a)]

-- | Builds a cap with a square centered on the end.
capSquare :: RealFloat n => n -> Point V2 n -> Point V2 n -> Point V2 n -> Trail V2 n
capSquare _r c a b = unLoc $ fromVertices [ a, a .+^ v, b .+^ v, b ]
  where
    v = perp (a .-. c)

-- | Builds an arc to fit with a given radius, center, start, and end points.
--   A Negative r means a counter-clockwise arc
capArc :: RealFloat n => n -> Point V2 n -> Point V2 n -> Point V2 n -> Trail V2 n
capArc r c a b = trailLike . moveTo c $ fs
  where
    fs | r < 0     = scale (-r) $ arcCW  (dirBetween a c) (dirBetween b c)
       | otherwise = scale r    $ arcCCW (dirBetween a c) (dirBetween b c)

-- | Join together a list of located trails with the given join style.  The
--   style is given as a function to compute the join given the local information
--   of the original vertex, the previous trail, and the next trail.  The result
--   is a single located trail.  A join radius is also given to aid in arc joins.
--
--   Note: this is not a general purpose join and assumes that we are joining an
--   offset trail.  For instance, a fixed radius arc will not fit between arbitrary
--   trails without trimming or extending.
joinSegments :: RealFloat n
             => n
             -> (n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n)
             -> Bool
             -> n
             -> n
             -> [Point V2 n]
             -> [Located (Trail V2 n)]
             -> Located (Trail V2 n)
joinSegments _ _ _ _ _ _ [] = mempty `at` origin
joinSegments _ _ _ _ _ [] _ = mempty `at` origin
joinSegments epsilon j isLoop ml r es ts@(t:_) = t'
  where
    t' | isLoop    = mapLoc (glueTrail . (<> f (take (length ts * 2 - 1) $ ss es (ts ++ [t])))) t
       | otherwise = mapLoc (<> f (ss es ts)) t
    ss es' ts' = concat [[test a b $ j ml r e a b, Just $ unLoc b] | (e,(a,b)) <- zip es' . (zip <*> tail) $ ts']
    test a b tj
        | atStart b `distance` atEnd a > epsilon = Just tj
        | otherwise                              = Nothing
    f = mconcat . catMaybes

-- | Take a join style and give the join function to be used by joinSegments.
fromLineJoin
  :: RealFloat n => LineJoin -> n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n
fromLineJoin j = case j of
    LineJoinMiter -> joinSegmentIntersect
    LineJoinRound -> joinSegmentArc
    LineJoinBevel -> joinSegmentClip

-- TODO: The joinSegmentCut option is not in our standard line joins.  I don't know
-- how useful it is graphically, I mostly had it as it was useful for debugging
{-
-- | Join with segments going back to the original corner.
joinSegmentCut :: (OrderedField n) => n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n
joinSegmentCut _ _ e a b = fromSegments
    [ straight (e .-. atEnd a)
    , straight (atStart b .-. e)
    ]
-}

-- | Join by directly connecting the end points.  On an inside corner this
--   creates negative space for even-odd fill.  Here is where we would want to
--   use an arc or something else in the future.
joinSegmentClip :: RealFloat n
  => n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n
joinSegmentClip _ _ _ a b = fromSegments [straight $ atStart b .-. atEnd a]

-- | Join with a radius arc.  On an inside corner this will loop around the interior
--   of the offset trail.  With a winding fill this will not be visible.
joinSegmentArc :: RealFloat n
  => n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n
joinSegmentArc _ r e a b = capArc r e (atEnd a) (atStart b)

-- | Join to the intersection of the incoming trails projected tangent to their ends.
--   If the intersection is beyond the miter limit times the radius, stop at the limit.
joinSegmentIntersect
    :: RealFloat n => n -> n -> Point V2 n -> Located (Trail V2 n) -> Located (Trail V2 n) -> Trail V2 n
joinSegmentIntersect miterLimit r e a b =
    if cross < 0.000001
      then clip
      else case traceP pa va t of
          -- clip join when we excede the miter limit.  We could instead
          -- Join at exactly the miter limit, but standard behavior seems
          -- to be clipping.
          Nothing -> clip
          Just p
            -- If trace gave us garbage...
            | p `distance` pb > abs (miterLimit * r) -> clip
            | otherwise                              -> unLoc $ fromVertices [ pa, p, pb ]
  where
    t = straight (miter vb) `at` pb
    va = unitPerp (pa .-. e)
    vb = negated $ unitPerp (pb .-. e)
    pa = atEnd a
    pb = atStart b
    miter v = abs (miterLimit * r) *^ v
    clip = joinSegmentClip miterLimit r e a b
    cross = let (xa,ya) = unr2 va; (xb,yb) = unr2 vb in abs (xa * yb - xb * ya)

