{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Shapes
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Various two-dimensional shapes.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Shapes
       (
         -- * Miscellaneous
         hrule, vrule

         -- * Regular polygons

       , regPoly
       , triangle
       , eqTriangle
       , square
       , pentagon
       , hexagon
       , heptagon
       , septagon
       , octagon
       , nonagon
       , decagon
       , hendecagon
       , dodecagon

         -- * Other special polygons
       , unitSquare
       , rect

         -- * Other shapes

       , roundedRect
       , RoundedRectOpts(..), radiusTL, radiusTR, radiusBL, radiusBR
       , roundedRect'
       ) where

import           Diagrams.Core

import           Diagrams.Angle
import           Diagrams.Located        (at)
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TrailLike
import           Diagrams.TwoD.Arc
import           Diagrams.TwoD.Polygons
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector

import           Diagrams.Util

import           Control.Lens            (makeLenses, op, (&), (.~), (<>~), (^.))
import           Data.Default.Class
import           Data.Semigroup
import           Data.VectorSpace

-- | Create a centered horizontal (L-R) line of the given length.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_hruleEx.svg#diagram=hruleEx&width=300>>
--
--   > hruleEx = vcat' (with & sep .~ 0.2) (map hrule [1..5])
--   >         # centerXY # pad 1.1
hrule :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
hrule d = trailLike $ trailFromSegments [straight $ r2 (d, 0)] `at` (p2 (-d/2,0))

-- | Create a centered vertical (T-B) line of the given length.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_vruleEx.svg#diagram=vruleEx&height=100>>
--
--   > vruleEx = hcat' (with & sep .~ 0.2) (map vrule [1, 1.2 .. 2])
--   >         # centerXY # pad 1.1
vrule :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
vrule d = trailLike $ trailFromSegments [straight $ r2 (0, (-d))] `at` (p2 (0,d/2))

-- | A square with its center at the origin and sides of length 1,
--   oriented parallel to the axes.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_unitSquareEx.svg#diagram=unitSquareEx&width=100>>
unitSquare :: (TrailLike t, V t ~ v, TwoD v) => t
unitSquare = polygon (def & polyType   .~ PolyRegular 4 (sqrt 2 / 2)
                          & polyOrient .~ OrientH)

-- > unitSquareEx = unitSquare # pad 1.1 # showOrigin

-- | A square with its center at the origin and sides of the given
--   length, oriented parallel to the axes.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_squareEx.svg#diagram=squareEx&width=200>>
square :: (TrailLike t, Transformable t, V t ~ v, TwoD v) => Scalar v -> t
square d = rect d d

-- > squareEx = hcat' (with & sep .~ 0.5) [square 1, square 2, square 3]
-- >          # centerXY # pad 1.1

-- | @rect w h@ is an axis-aligned rectangle of width @w@ and height
--   @h@, centered at the origin.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_rectEx.svg#diagram=rectEx&width=150>>
rect :: (TrailLike t, Transformable t, V t ~ v, TwoD v) => Scalar v -> Scalar v -> t
rect w h = trailLike . head . op Path $ unitSquare # scaleX w # scaleY h

-- > rectEx = rect 1 0.7 # pad 1.1

    -- The above may seem a bit roundabout.  In fact, we used to have
    --
    --   rect w h = unitSquare # scaleX w # scaleY h
    --
    -- since unitSquare can produce any TrailLike.  The current code
    -- instead uses (unitSquare # scaleX w # scaleY h) to specifically
    -- produce a Path, which is then deconstructed and passed back into
    -- 'trailLike' to create any TrailLike.
    --
    -- The difference is that while scaling by zero works fine for
    -- Path it does not work very well for, say, Diagrams (leading to
    -- NaNs or worse).  This way, we force the scaling to happen on a
    -- Path, where we know it will behave properly, and then use the
    -- resulting geometry to construct an arbitrary TrailLike.
    --
    -- See https://github.com/diagrams/diagrams-lib/issues/43 .

------------------------------------------------------------
--  Regular polygons
------------------------------------------------------------

-- | Create a regular polygon. The first argument is the number of
--   sides, and the second is the /length/ of the sides. (Compare to the
--   'polygon' function with a 'PolyRegular' option, which produces
--   polygons of a given /radius/).
--
--   The polygon will be oriented with one edge parallel to the x-axis.
regPoly :: (TrailLike t, V t ~ v, TwoD v) => Int -> Scalar v -> t
regPoly n l = polygon (def & polyType .~
                               PolySides
                                 (repeat (1/fromIntegral n @@ turn))
                                 (replicate (n-1) l)
                           & polyOrient .~ OrientH
                           )

-- > shapeEx sh   = sh 1 # pad 1.1
-- > triangleEx   = shapeEx triangle
-- > pentagonEx   = shapeEx pentagon
-- > hexagonEx    = shapeEx hexagon
-- > heptagonEx   = shapeEx heptagon
-- > octagonEx    = shapeEx octagon
-- > nonagonEx    = shapeEx nonagon
-- > decagonEx    = shapeEx decagon
-- > hendecagonEx = shapeEx hendecagon
-- > dodecagonEx  = shapeEx dodecagon

-- | A synonym for 'triangle', provided for backwards compatibility.
eqTriangle :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
eqTriangle = triangle

-- | An equilateral triangle, with sides of the given length and base
--   parallel to the x-axis.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_triangleEx.svg#diagram=triangleEx&width=100>>
triangle :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
triangle = regPoly 3

-- | A regular pentagon, with sides of the given length and base
--   parallel to the x-axis.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_pentagonEx.svg#diagram=pentagonEx&width=100>>
pentagon :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
pentagon = regPoly 5

-- | A regular hexagon, with sides of the given length and base
--   parallel to the x-axis.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_hexagonEx.svg#diagram=hexagonEx&width=100>>
hexagon :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
hexagon = regPoly 6

-- | A regular heptagon, with sides of the given length and base
--   parallel to the x-axis.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_heptagonEx.svg#diagram=heptagonEx&width=100>>
heptagon :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
heptagon = regPoly 7

-- | A synonym for 'heptagon'.  It is, however, completely inferior,
--   being a base admixture of the Latin /septum/ (seven) and the
--   Greek γωνία (angle).
septagon :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
septagon = heptagon

-- | A regular octagon, with sides of the given length and base
--   parallel to the x-axis.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_octagonEx.svg#diagram=octagonEx&width=100>>
octagon :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
octagon = regPoly 8

-- | A regular nonagon, with sides of the given length and base
--   parallel to the x-axis.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_nonagonEx.svg#diagram=nonagonEx&width=100>>
nonagon :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
nonagon = regPoly 9

-- | A regular decagon, with sides of the given length and base
--   parallel to the x-axis.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_decagonEx.svg#diagram=decagonEx&width=100>>
decagon :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
decagon = regPoly 10

-- | A regular hendecagon, with sides of the given length and base
--   parallel to the x-axis.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_hendecagonEx.svg#diagram=hendecagonEx&width=100>>
hendecagon :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
hendecagon = regPoly 11

-- | A regular dodecagon, with sides of the given length and base
--   parallel to the x-axis.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_dodecagonEx.svg#diagram=dodecagonEx&width=100>>
dodecagon :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> t
dodecagon = regPoly 12

------------------------------------------------------------
--  Other shapes  ------------------------------------------
------------------------------------------------------------
data RoundedRectOpts d = RoundedRectOpts { _radiusTL :: d
                                       , _radiusTR   :: d
                                       , _radiusBL   :: d
                                       , _radiusBR   :: d
                                       }

makeLenses ''RoundedRectOpts

instance (Num d) => Default (RoundedRectOpts d) where
  def = RoundedRectOpts 0 0 0 0

-- | @roundedRect w h r@ generates a closed trail, or closed path
--   centered at the origin, of an axis-aligned rectangle with width
--   @w@, height @h@, and circular rounded corners of radius @r@.  If
--   @r@ is negative the corner will be cut out in a reverse arc. If
--   the size of @r@ is larger than half the smaller dimension of @w@
--   and @h@, then it will be reduced to fit in that range, to prevent
--   the corners from overlapping.  The trail or path begins with the
--   right edge and proceeds counterclockwise.  If you need to specify
--   a different radius for each corner individually, use
--   'roundedRect'' instead.
--
--   <<diagrams/src_Diagrams_TwoD_Shapes_roundedRectEx.svg#diagram=roundedRectEx&width=400>>
--
--   > roundedRectEx = pad 1.1 . centerXY $ hcat' (with & sep .~ 0.2)
--   >   [ roundedRect  0.5 0.4 0.1
--   >   , roundedRect  0.5 0.4 (-0.1)
--   >   , roundedRect' 0.7 0.4 (with & radiusTL .~ 0.2
--   >                                & radiusTR .~ -0.2
--   >                                & radiusBR .~ 0.1)
--   >   ]

roundedRect :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> Scalar v -> Scalar v -> t
roundedRect w h r = roundedRect' w h (def & radiusTL .~ r
                                          & radiusBR .~ r
                                          & radiusTR .~ r
                                          & radiusBL .~ r)

-- | @roundedRect'@ works like @roundedRect@ but allows you to set the radius of
--   each corner indivually, using @RoundedRectOpts@. The default corner radius is 0.
--   Each radius can also be negative, which results in the curves being reversed
--   to be inward instead of outward.
roundedRect' :: (TrailLike t, V t ~ v, TwoD v) => Scalar v -> Scalar v -> RoundedRectOpts (Scalar v) -> t
roundedRect' w h opts
   = trailLike
   . (`at` (p2 (w/2, abs rBR - h/2)))
   . wrapTrail
   . glueLine
   $ seg (0, h - abs rTR - abs rBR)
   <> mkCorner 0 rTR
   <> seg (abs rTR + abs rTL - w, 0)
   <> mkCorner 1 rTL
   <> seg (0, abs rTL + abs rBL - h)
   <> mkCorner 2 rBL
   <> seg (w - abs rBL - abs rBR, 0)
   <> mkCorner 3 rBR
  where seg   = lineFromOffsets . (:[]) . r2
        diag  = sqrt (w * w + h * h)
        -- to clamp corner radius, need to compare with other corners that share an
        -- edge. If the corners overlap then reduce the largest corner first, as far
        -- as 50% of the edge in question.
        rTL                 = clampCnr radiusTR radiusBL radiusBR radiusTL
        rBL                 = clampCnr radiusBR radiusTL radiusTR radiusBL
        rTR                 = clampCnr radiusTL radiusBR radiusBL radiusTR
        rBR                 = clampCnr radiusBL radiusTR radiusTL radiusBR
        clampCnr rx ry ro r = let (rx',ry',ro',r') = (opts^.rx, opts^.ry, opts^.ro, opts^.r)
                                in clampDiag ro' . clampAdj h ry' . clampAdj w rx' $ r'
        -- prevent curves of adjacent corners from overlapping
        clampAdj len adj r  = if abs r > len/2
                                then sign r * max (len/2) (min (len - abs adj) (abs r))
                                else r
        -- prevent inward curves of diagonally opposite corners from intersecting
        clampDiag opp r     = if r < 0 && opp < 0 && abs r > diag / 2
                                then sign r * max (diag / 2) (min (abs r) (diag + opp))
                                else r
        sign n = if n < 0 then -1 else 1
        mkCorner k r | r == 0    = mempty
                     | r < 0     = doArc 3 (-1)
                     | otherwise = doArc 0 1
                     where
                       doArc d s =
                           arc' r (xDir & _theta <>~ ((k+d)/4 @@ turn)) (s/4 @@ turn)
