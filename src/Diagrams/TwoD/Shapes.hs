{-# LANGUAGE TypeFamilies
           , FlexibleContexts
  #-}

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
       , eqTriangle
       , square
       , pentagon
       , hexagon
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
       , RoundedRectOpts(..)
       , roundedRect'
       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Segment
import Diagrams.Path
import Diagrams.TwoD.Arc
import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Polygons

import Diagrams.Util

import Data.Default
import Data.Semigroup
import Data.VectorSpace

-- | Create a centered horizontal (L-R) line of the given length.
hrule :: (PathLike p, V p ~ R2) => Double -> p
hrule d = pathLike (P (-d/2,0)) False [Linear (d,0)]

-- | Create a centered vertical (T-B) line of the given length.
vrule :: (PathLike p, V p ~ R2) => Double -> p
vrule d = pathLike (P (0,d/2)) False [Linear (0,-d)]

-- | A sqaure with its center at the origin and sides of length 1,
--   oriented parallel to the axes.
unitSquare :: (PathLike p, V p ~ R2) => p
unitSquare = polygon with { polyType   = PolyRegular 4 (sqrt 2 / 2)
                          , polyOrient = OrientH }

-- | A sqaure with its center at the origin and sides of the given
--   length, oriented parallel to the axes.
square :: (PathLike p, Transformable p, V p ~ R2) => Double -> p
square d = unitSquare # scale d

-- | @rect w h@ is an axis-aligned rectangle of width @w@ and height
--   @h@, centered at the origin.
rect :: (PathLike p, Transformable p, V p ~ R2) => Double -> Double -> p
rect w h = unitSquare # scaleX w # scaleY h

------------------------------------------------------------
--  Regular polygons
------------------------------------------------------------

-- | Create a regular polygon. The first argument is the number of
--   sides, and the second is the /length/ of the sides. (Compare to the
--   'polygon' function with a 'PolyRegular' option, which produces
--   polygons of a given /radius/).
--
--   The polygon will be oriented with one edge parallel to the x-axis.
regPoly :: (PathLike p, V p ~ R2) => Int -> Double -> p
regPoly n l = polygon with { polyType =
                               PolySides
                                 (repeat (1/ fromIntegral n :: CircleFrac))
                                 (replicate (n-1) l)
                           , polyOrient = OrientH
                           }

-- | An equilateral triangle, with sides of the given length and base parallel
--   to the x-axis.
eqTriangle :: (PathLike p, V p ~ R2) => Double -> p
eqTriangle = regPoly 3

-- | A regular pentagon, with sides of the given length and base
--   parallel to the x-axis.
pentagon :: (PathLike p, V p ~ R2) => Double -> p
pentagon = regPoly 5

-- | A regular hexagon, with sides of the given length and base
--   parallel to the x-axis.
hexagon :: (PathLike p, V p ~ R2) => Double -> p
hexagon = regPoly 6

-- | A regular septagon, with sides of the given length and base
--   parallel to the x-axis.
septagon :: (PathLike p, V p ~ R2) => Double -> p
septagon = regPoly 7

-- | A regular octagon, with sides of the given length and base
--   parallel to the x-axis.
octagon :: (PathLike p, V p ~ R2) => Double -> p
octagon = regPoly 8

-- | A regular nonagon, with sides of the given length and base
--   parallel to the x-axis.
nonagon :: (PathLike p, V p ~ R2) => Double -> p
nonagon = regPoly 9

-- | A regular decagon, with sides of the given length and base
--   parallel to the x-axis.
decagon :: (PathLike p, V p ~ R2) => Double -> p
decagon = regPoly 10

-- | A regular hendecagon, with sides of the given length and base
--   parallel to the x-axis.
hendecagon :: (PathLike p, V p ~ R2) => Double -> p
hendecagon = regPoly 11

-- | A regular dodecagon, with sides of the given length and base
--   parallel to the x-axis.
dodecagon :: (PathLike p, V p ~ R2) => Double -> p
dodecagon = regPoly 12

------------------------------------------------------------
--  Other shapes  ------------------------------------------
------------------------------------------------------------

-- | @roundedRect v r@ generates a closed trail, or closed path
-- centered at the origin, of an axis-aligned rectangle with diagonal
-- @v@ and circular rounded corners of radius @r@.  @r@ must be
-- between @0@ and half the smaller dimension of @v@, inclusive; smaller or
-- larger values of @r@ will be treated as @0@ or half the smaller
-- dimension of @v@, respectively.  The trail or path begins with the
-- right edge and proceeds counterclockwise.
roundedRect :: (PathLike p, V p ~ R2) => R2 -> Double -> p
roundedRect v r = roundedRect' (with { radiusTL = abs r, 
                                       radiusBR = abs r, 
                                       radiusTR = abs r, 
                                       radiusBL = abs r}) v


-- | @roundedRect'@ works like @roundedRect@ but allows you to set the radius of 
--   each corner indivually, using RoundedRectOpts. The default corner radius is 0.
--   Each radius can also be negative, which results in the curves being reversed 
--   to be inward instead of outward.
roundedRect' :: (PathLike p, V p ~ R2) => RoundedRectOpts -> R2 -> p
roundedRect' opts (w,h) = pathLike (P (w/2, (abs rBR) - h/2)) True
                        . trailSegments
                        $ seg (0, h - (abs rTR) - (abs rBR))
                        <> mkCorner 0 rTR
                        <> seg ((abs rTR) + (abs rTL) - w,0)
                        <> mkCorner 1 rTL
                        <> seg (0, (abs rTL) + (abs rBL) - h)
                        <> mkCorner 2 rBL
                        <> seg (w - (abs rBL) - (abs rBR),0)
                        <> mkCorner 3 rBR
  where seg = fromOffsets . (:[])
        -- to clamp corner radius, need to compare with other corners that share an
        -- edge. If the corners overlap then reduce the largest corner first, as far
        -- as 50% of the edge in question.
        rTL                 = clampCnr radiusTL radiusTR radiusBL
        rBL                 = clampCnr radiusBL radiusBR radiusTL
        rTR                 = clampCnr radiusTR radiusTL radiusBR
        rBR                 = clampCnr radiusBR radiusBL radiusTR
        clampCnr r rx ry    = let (r',rx',ry') = (r opts, rx opts, ry opts)  
                                in clampAxis (clampAxis r' (w - abs rx') w) (h - abs ry') h
        clampAxis r rem len = if abs r > len/2 
                                then sign r * (max (len/2) $ min (abs r) rem)
                                else r
        sign n = if n < 0 then -1 else 1
        mkCorner k r | r == 0    = mempty
                     | r < 0     = doArc 3 2
                     | otherwise = doArc 0 1
                     where doArc d d' = arc ((k+d)/4) ((k+d')/4:: CircleFrac) # scale (abs r)


data RoundedRectOpts = RoundedRectOpts { radiusTL :: Double
                                       , radiusTR :: Double
                                       , radiusBL :: Double
                                       , radiusBR :: Double
                                       }
instance Default RoundedRectOpts where
  def = RoundedRectOpts 0 0 0 0
