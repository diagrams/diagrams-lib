{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.CubicSpline.Boehm
-- Copyright   :  (c) 2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Boehm's algorithm for converting a cubic B-spline into a sequence
-- of cubic Bezier curves.
--
-----------------------------------------------------------------------------
module Diagrams.CubicSpline.Boehm
       ( BSpline
       , bsplineToBeziers
       , bspline
       ) where

import           Control.Arrow       ((***))
import           Data.List           (sort, tails)
import           Diagrams.Core       (N, Point, V, origin)
import           Diagrams.Located    (at, loc, unLoc)
import           Diagrams.Segment    (FixedSegment (..), fromFixedSeg)
import           Diagrams.TrailLike  (TrailLike, fromLocSegments)
import           Diagrams.TwoD.Types (P2, V2)
import           Diagrams.Util       (iterateN)
import           Linear.Vector       (Additive, lerp)

type BSpline v n = [Point v n]

-- | @affineCombo a b t x y@ computes an affine combination of x and y
--   which lies at parameter t, if x has parameter a and y has parameter b.
--   The usual @lerp@ arises by giving x parameter 0 and y parameter 1.
affineCombo :: (Additive f, Fractional a) => a -> a -> a -> f a -> f a -> f a
affineCombo a b t x y = lerp ((t-a)/(b-a)) y x

-- | @windows k xs@ yields all the length-@k@ windows from @xs@, e.g.
--   @windows 3 [a,b,c,d,e] == [[a,b,c], [b,c,d], [c,d,e]]@.
windows :: Int -> [a] -> [[a]]
windows k = takeWhile ((==k) . length) . map (take k) . tails

-- | @extend k xs@ extends @xs@ on both ends by prepending @k@ copies
-- of its head and appending @k@ copies of its last element.  For example,
-- @extend 2 [1..5] == [1,1,1,2,3,4,5,5,5]@.
extend :: Int -> [a] -> [a]
extend k xs = replicate k (head xs) ++ xs ++ replicate k (last xs)

-- | A "polar point" is a point along with three knot values.
--   We consider the "blossom" of a cubic spline, a 3-ary symmetric
--   polynomial (see XXX); a polar point consists of 3 values paired
--   with the output of the blossom at those input values.  Blossoms
--   have nice affine properties so this makes it easy to keep track
--   of how points may be combined to yield other points of interest.
--
--   Invariant: knot values are in nondecreasing order.
data PolarPt v n = PP { unPP :: Point v n, knots :: [n] }

mkPolarPt :: Ord n => Point v n -> [n] -> PolarPt v n
mkPolarPt pt kts = PP pt (sort kts)

-- | Precondition: the knots of the two polar points overlap, like abc and bcd.
--   The Int should be 0 or 1, indicating which knot to replicate (0
--   means to replicate b, yielding bbc, 1 means to replicate c,
--   yielding bcc).
combine
  :: (Additive v, Fractional n, Ord n)
  => Int -> PolarPt v n -> PolarPt v n -> PolarPt v n
combine k (PP pt1 kts1) (PP pt2 kts2)
  = mkPolarPt
      (affineCombo (head kts1) (last kts2) newKt pt1 pt2)
      (newKt : drop 1 kts1)
  where
    newKt = kts2 !! k

-- | Convert a uniform cubic B-spline to a sequence of cubic beziers.
--   (/Uniform/ refers to the fact that the knots are assumed to be
--   evenly spaced, with no duplicates.)  The knots at the end are
--   replicated so the cubic spline begins and ends at the first and
--   last control points, tangent to the line from the end control
--   point to the next.
bsplineToBeziers
  :: (Additive v, Fractional n, Num n, Ord n)
  => BSpline v n
  -> [FixedSegment v n]
bsplineToBeziers controls = beziers
  where
    n                            = length controls
    numKnots                     = n + 2
    knots                        = iterateN numKnots (+1/(fromIntegral numKnots - 1)) 0

    -- The control points are P(a,b,c), P(b,c,d), P(c,d,e), and so on.
    controls' = zipWith mkPolarPt (extend 2 controls) (windows 3 $ extend 2 knots)

    -- The bezier internal control points are affine combinations of
    -- the spline control points.
    bezierControls        = map combineC (windows 2 controls')
    combineC [pabc, pbcd] = (combine 0 pabc pbcd, combine 1 pabc pbcd)

    -- The bezier end points are affine combinations of the bezier
    -- control points.
    bezierEnds                   = map combineE (windows 2 bezierControls)
    combineE [(_,pabb),(pbbc,_)] = combine 0 pabb pbbc

    -- Finally, we actually put together the generated bezier segments.
    beziers                      = zipWith mkBezier (drop 1 bezierControls) (windows 2 bezierEnds)
      where
        mkBezier (paab,pabb) [paaa,pbbb]
          = FCubic (unPP paaa) (unPP paab) (unPP pabb) (unPP pbbb)

-- | Generate a uniform cubic B-spline from the given control points.
--   The spline starts and ends at the first and last control points,
--   and is tangent to the line to the second(-to-last) control point.
--   It does not necessarily pass through any of the other control
--   points.
bspline :: (TrailLike t, V t ~ v, N t ~ n) => BSpline v n -> t
bspline = fromLocSegments . fixup . map fromFixedSeg . bsplineToBeziers
  where
    fixup []        = [] `at` origin
    fixup (b1:rest) = (unLoc b1 : map unLoc rest) `at` loc b1
