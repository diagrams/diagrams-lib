{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , ViewPatterns
  #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Diagrams.Core

import Diagrams.Coordinates
import Diagrams.Path
import Diagrams.Segment
import Diagrams.TwoD.Arc
import Diagrams.TwoD.Polygons
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Types

import Diagrams.Util

import Data.Default
import Data.Semigroup
import Data.VectorSpace (Scalar(..), InnerSpace(..))
import Data.Basis       (HasBasis(..), Basis(..))
import Data.MemoTrie    (HasTrie(..))

-- | Create a centered horizontal (L-R) line of the given length.
hrule :: (Fractional a, PathLike p, V p ~ D2 a) => a -> p
hrule d = pathLike (p2 (-d/2,0)) False [Linear (d & 0)]

-- | Create a centered vertical (T-B) line of the given length.
vrule :: (Fractional a, PathLike p, V p ~ D2 a) => a -> p
vrule d = pathLike (p2 (0,d/2)) False [Linear (0 & (-d))]

-- | A sqaure with its center at the origin and sides of length 1,
--   oriented parallel to the axes.
unitSquare :: ( Ord a
              , Floating a
              , HasBasis a
              , InnerSpace a
              , HasTrie (Basis a)
              , a ~ Scalar a
              , PathLike p
              , V p ~ D2 a
              ) => p
unitSquare = polygon with { polyType   = PolyRegular 4 (sqrt 2 / 2)
                          , polyOrient = OrientH }

-- | A sqaure with its center at the origin and sides of the given
--   length, oriented parallel to the axes.
square :: ( Ord a
          , Floating a
          , HasBasis a
          , InnerSpace a
          , HasTrie (Basis a)
          , a ~ Scalar a
          , PathLike p
          , Transformable p
          , V p ~ D2 a
          ) => a -> p
square d = unitSquare # scale d

-- | @rect w h@ is an axis-aligned rectangle of width @w@ and height
--   @h@, centered at the origin.
rect :: ( Ord a
        , Floating a
        , HasBasis a
        , InnerSpace a
        , HasTrie (Basis a)
        , a ~ Scalar a
        , PathLike p
        , Transformable p
        , V p ~ D2 a
        ) => a -> a -> p
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
regPoly :: forall a p. ( Floating a
                       , Ord a
                       , InnerSpace a
                       , HasTrie (Basis a)
                       , HasBasis a
                       , a ~ Scalar a
                       , PathLike p
                       , V p ~ D2 a
                       ) => Int -> a -> p
regPoly n l = polygon with { polyType =
                               PolySides
                                 (repeat (1/ fromIntegral n :: CircleFrac a))
                                 (replicate (n-1) l)
                           , polyOrient = OrientH
                           }

-- | An equilateral triangle, with sides of the given length and base parallel
--   to the x-axis.
eqTriangle :: ( Floating a
              , Ord a
              , InnerSpace a
              , HasTrie (Basis a)
              , HasBasis a
              , a ~ Scalar a
              , PathLike p
              , V p ~ D2 a
              ) => a -> p
eqTriangle = regPoly 3

-- | A regular pentagon, with sides of the given length and base
--   parallel to the x-axis.
pentagon :: ( Floating a
            , Ord a
            , InnerSpace a
            , HasTrie (Basis a)
            , HasBasis a
            , a ~ Scalar a
            , PathLike p
            , V p ~ D2 a
            ) => a -> p
pentagon = regPoly 5

-- | A regular hexagon, with sides of the given length and base
--   parallel to the x-axis.
hexagon :: ( Floating a
           , Ord a
           , InnerSpace a
           , HasTrie (Basis a)
           , HasBasis a
           , a ~ Scalar a
           , PathLike p
           , V p ~ D2 a
           ) => a -> p
hexagon = regPoly 6

-- | A regular septagon, with sides of the given length and base
--   parallel to the x-axis.
septagon :: ( Floating a
            , Ord a
            , InnerSpace a
            , HasTrie (Basis a)
            , HasBasis a
            , a ~ Scalar a
            , PathLike p
            , V p ~ D2 a
            ) => a -> p
septagon = regPoly 7

-- | A regular octagon, with sides of the given length and base
--   parallel to the x-axis.
octagon :: ( Floating a
           , Ord a
           , InnerSpace a
           , HasTrie (Basis a)
           , HasBasis a
           , a ~ Scalar a
           , PathLike p
           , V p ~ D2 a
           ) => a -> p
octagon = regPoly 8

-- | A regular nonagon, with sides of the given length and base
--   parallel to the x-axis.
nonagon :: ( Floating a
           , Ord a
           , InnerSpace a
           , HasTrie (Basis a)
           , HasBasis a
           , a ~ Scalar a
           , PathLike p
           , V p ~ D2 a
           ) => a -> p
nonagon = regPoly 9

-- | A regular decagon, with sides of the given length and base
--   parallel to the x-axis.
decagon :: ( Floating a
           , Ord a
           , InnerSpace a
           , HasTrie (Basis a)
           , HasBasis a
           , a ~ Scalar a
           , PathLike p
           , V p ~ D2 a
           ) => a -> p
decagon = regPoly 10

-- | A regular hendecagon, with sides of the given length and base
--   parallel to the x-axis.
hendecagon :: ( Floating a
              , Ord a
              , InnerSpace a
              , HasTrie (Basis a)
              , HasBasis a
              , a ~ Scalar a
              , PathLike p
              , V p ~ D2 a
              ) => a -> p
hendecagon = regPoly 11

-- | A regular dodecagon, with sides of the given length and base
--   parallel to the x-axis.
dodecagon :: ( Floating a
             , Ord a
             , InnerSpace a
             , HasTrie (Basis a)
             , HasBasis a
             , a ~ Scalar a
             , PathLike p
             , V p ~ D2 a
             ) => a -> p
dodecagon = regPoly 12

------------------------------------------------------------
--  Other shapes  ------------------------------------------
------------------------------------------------------------

-- | @roundedRect w h r@ generates a closed trail, or closed path
--   centered at the origin, of an axis-aligned rectangle with width
--   @w@, height @h@, and circular rounded corners of radius @r@.  If
--   @r@ is negative the corner will be cut out in a reverse arc. If
--   the size of @r@ is larger than half the smaller dimension of @w@
--   and @h@, then it will be reduced to fit in that range, to prevent
--   the corners from overlapping.  The trail or path begins with the
--   right edge and proceeds counterclockwise.  If you need to specify
--   a different radius for each corner individually, use
--   @roundedRect'@ instead.
roundedRect :: ( Ord a
               , Floating a
               , HasBasis a
               , HasTrie (Basis a)
               , a ~ Scalar a
               , PathLike p
               , V p ~ D2 a
               ) => a -> a -> a -> p
roundedRect w h r = roundedRect' w h (with { radiusTL = r,
                                             radiusBR = r,
                                             radiusTR = r,
                                             radiusBL = r})

-- | @roundedRect'@ works like @roundedRect@ but allows you to set the radius of
--   each corner indivually, using @RoundedRectOpts@. The default corner radius is 0.
--   Each radius can also be negative, which results in the curves being reversed
--   to be inward instead of outward.
roundedRect' :: forall a p. ( Ord a
                            , Floating a
                            , HasBasis a
                            , HasTrie (Basis a)
                            , a ~ Scalar a
                            , PathLike p
                            , V p ~ D2 a
                            ) => a -> a -> RoundedRectOpts a -> p
roundedRect' w h opts
   = pathLike (p2 (w/2, abs rBR - h/2)) True
   . trailSegments
   $ seg (0, h - abs rTR - abs rBR)
   <> mkCorner 0 rTR
   <> seg (abs rTR + abs rTL - w, 0)
   <> mkCorner 1 rTL
   <> seg (0, abs rTL + abs rBL - h)
   <> mkCorner 2 rBL
   <> seg (w - abs rBL - abs rBR, 0)
   <> mkCorner 3 rBR
  where seg   = fromOffsets . (:[]) . d2
        diag  = sqrt (w * w + h * h)
        -- to clamp corner radius, need to compare with other corners that share an
        -- edge. If the corners overlap then reduce the largest corner first, as far
        -- as 50% of the edge in question.
        rTL                 = clampCnr radiusTR radiusBL radiusBR radiusTL
        rBL                 = clampCnr radiusBR radiusTL radiusTR radiusBL
        rTR                 = clampCnr radiusTL radiusBR radiusBL radiusTR
        rBR                 = clampCnr radiusBL radiusTR radiusTL radiusBR
        clampCnr rx ry ro r = let (rx',ry',ro',r') = (rx opts, ry opts, ro opts, r opts)
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
                     | r < 0     = doArc 3 2
                     | otherwise = doArc 0 1
          where doArc d d' = arc ((k+d)/4) ((k+d')/4:: CircleFrac a) # scale (abs r)

data RoundedRectOpts a = RoundedRectOpts { radiusTL :: a
                                         , radiusTR :: a
                                         , radiusBL :: a
                                         , radiusBR :: a
                                         }

instance (Num a) => Default (RoundedRectOpts a) where
  def = RoundedRectOpts 0 0 0 0
