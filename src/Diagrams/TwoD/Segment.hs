{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Orphan Traced instances for Segment Closed R2 and FixedSegment R2.
-- They can't go in Traced; but they shouldn't really go in
-- Diagrams.Segment either because we only have Traced instances for
-- the special case of R2.
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Segment
-- Copyright   :  (c) 2012 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Segments in two dimensions are special since we may meaningfully
-- compute their point of intersection with a ray.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Segment
  ( segmentIntersection
  , convexHull2D
  , lineSegmentIntersect
  , closest
  )
  where

import           Data.List               (sort)
import           Data.Maybe
import           Data.Foldable           (Foldable)
import           Control.Applicative
import           Control.Lens            hiding (( # ), at, contains)
import           Control.Monad           (guard)

import           Diagrams.Core

import           Diagrams.Angle
import           Diagrams.BoundingBox
import           Diagrams.TwoD.Segment.Bernstein
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Segment
import           Diagrams.Solve
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types     hiding (p2)
import           Diagrams.TwoD.Vector
import           Diagrams.Util

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

{- All instances of Traced should maintain the invariant that the list of
   traces is sorted in increasing order.
-}

instance RealFloat n => Traced (Segment Closed V2 n) where
  getTrace = getTrace . mkFixedSeg . (`at` origin)

instance RealFloat n => Traced (FixedSegment V2 n) where

{- Given lines defined by p0 + t0 * v0 and p1 + t1 * v1, their point of
   intersection in 2D is given by

     t_i = (v_(1-i)^ . (p1 - p0)) / (v1^ . v0)

   where v^ denotes the perpendicular to v, i.e. v rotated by
   -tau/4.

   This can be derived by starting with the parametric equation

     p0 + v0 t0 = p1 + v1 t1

   and rearranging to get the matrix equation

     [v0 -v1] [ t0 ]  =  (p1 - p0)
              [ t1 ]

   Working out the product of the inverse of [v0 -v1] with (p1 - p0)
   results in the above formulas for t_i.
-}

  getTrace (FLinear p0 p0') = mkTrace $ \p1 v1 ->
    let
      v0     = p0' .-. p0
      det    = perp v1 `dot` v0
      p      = p1 .-. p0
      t0     = (perp v1 `dot` p) / det
      t1     = (perp v0 `dot` p) / det
    in
      mkSortedList $
        if det == 0 || t0 < 0 || t0 > 1
          then []
          else [t1]

{- To do intersection of a line with a cubic Bezier, we first rotate
   and scale everything so that the line has parameters (origin, unitX);
   then we find the intersection(s) of the Bezier with the x-axis.

   XXX could we speed this up by first checking whether all the
   control point y-coordinates lie on the same side of the x-axis (if so,
   there can't possibly be any intersections)?  Need to set up some
   benchmarks.
-}

  getTrace bez@(FCubic {}) = mkTrace $ \p1 v1 ->
    let
      bez'@(FCubic x1 c1 c2 x2) =
        bez # moveOriginTo p1
            # rotate (negated (v1^._theta))
            # scale (1/norm v1)
      [y0,y1,y2,y3] = map (snd . unp2) [x1,c1,c2,x2]
      a  = -y0 + 3*y1 - 3*y2 + y3
      b  = 3*y0 - 6*y1 + 3*y2
      c  = -3*y0 + 3*y1
      d  = y0
      ts = filter (liftA2 (&&) (>= 0) (<= 1)) (cubForm a b c d)
      xs = map (fst . unp2 . atParam bez') ts
    in
      mkSortedList xs



-- pathIntersections :: OrderedField n => Path V2 n -> Path V2 n -> [P2 n]
-- pathIntersections as bs = do
--   a <- pathTrails as
--   b <- pathTrails bs
--   locTrailIntersections a b
-- 
-- locTrailIntersections :: OrderedField n
--   => Located (Trail V2 n) -> Located (Trail V2 n) -> [P2 n]
-- locTrailIntersections as bs = do
--   a <- fixTrail as
--   b <- fixTrail bs
--   segmentIntersection a b

segmentIntersection :: OrderedField n => FixedSegment V2 n -> FixedSegment V2 n -> [P2 n]
segmentIntersection s1 s2 = 
  case (s1,s2) of
    (FCubic {}  , FCubic {})       -> map (atParam s1 . fst) $ bezierClip 1e-6 s1 s2
    (FCubic {}  , FLinear p q)     -> filter (lineTest p q) (lineSegmentIntersect p q s1)
    (FLinear p q, FCubic {})       -> filter (lineTest p q) (lineSegmentIntersect p q s2)
    (FLinear p1 p2, FLinear p3 p4) ->
      let p0  = lineLineIntersect p1 p2 p3 p4
          box = segmentIntersectingBox s1 s2

      in  guard (box `contains` p0) *> pure p0
    where
      lineTest p q p0 = fromCorners p q `contains` p0

-- | Find the convex hull of a list of points using Andrew's monotone chain
--   algorithm O(n log n).
--   
--   Returns clockwise list of points starting from the left-most point.
--
--   If your list of points is already sorted in the x direction see
--   'sortedConvexHull'.
convexHull2D :: OrderedField n => [P2 n] -> [P2 n]
convexHull2D ps = init upper ++ reverse (tail lower)
  where
    (lower, upper) = sortedConvexHull (sort ps)

------------------------------------------------------------------------
-- Low level functions
------------------------------------------------------------------------

-- | Intersections of an infinite line going through points @p@ and @q@ and the segment.
lineSegmentIntersect :: OrderedField n => P2 n -> P2 n -> FixedSegment V2 n -> [P2 n]
lineSegmentIntersect p q (FLinear p0 p1) = [lineLineIntersect p0 p1 p q] 
lineSegmentIntersect p q b               = map (b `atParam`) params
  where
    params = bezierFindRoot (listToBernstein $ map (view _y) [p0, p1, p2, p3]) 0 1 1e-6
    V2 x y = p .-. q
    FCubic p0 p1 p2 p3 = rotate (negated $ atan2A' y x) . moveOriginTo p $ b

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

-- type Line v n = Located (v n)

-- pointLineDistance :: Floating n => P2 n -> P2 n -> P2 n -> n
-- pointLineDistance (P (V2 x1 y1)) (P (V2 x2 y2)) (P (V2 x0 y0))
--   = abs (dy * x0 - dx * y0 - x1 * y2 + x2 * y1) / sqrt (dx * dx + dy * dy)
--   where
--     dx = x2 - x1
--     dy = y2 - y1

-- lineEq :: (Metric v, Floating n) => Line v n -> (v n,n)
-- lineEq (viewLoc -> (p,v)) = (v',c)
--   where
--     v' = signorm v
--     c  = 

-- | Returns @(a, b, c)@ such that @ax + by + c = 0@ is the line going through 
--   @p1@ and @p2@ with @a^2 + b^2 = 1@.
lineEquation :: Floating n => P2 n -> P2 n -> (n, n, n)
lineEquation (P (V2 x1 y1)) (P (V2 x2 y2)) = (a, b, c)
  where
    a  = a' / d
    b  = b' / d
    c  = -(y1*b' + x1*a') / d
    a' = y1 - y2
    b' = x2 - x1
    d  = sqrt $ a'*a' + b'*b'

-- | Return the the distance from a point to the line.
lineDistance :: Floating n => P2 n -> P2 n -> P2 n -> n
lineDistance p1 p2 (P (V2 x y)) = a*x + b*y + c
  where (a, b, c) = lineEquation p1 p2

-- find the x value where the line through the two points
-- intersect the line y=d
intersectPt :: OrderedField n => n -> P2 n -> P2 n -> n
intersectPt d (P (V2 x1 y1)) (P (V2 x2 y2)) =
  x1 + (d - y1) * (x2 - x1) / (y2 - y1)

cross2 :: Num n => V2 n -> V2 n -> n
cross2 (V2 x1 y1) (V2 x2 y2) = x1 * y2 - y1 * x2

-- clockwise :: (Num n, Ord n) => V2 n -> V2 n -> Bool
-- clockwise a b = a `cross2` b <= 0

avg :: Fractional n => n -> n -> n
avg a b = (a + b)/2

------------------------------------------------------------------------
-- Low level
------------------------------------------------------------------------

-- | Use the bezier clipping algorithm to return the parameters at which the 
--   bezier curves intersect.
bezierClip :: OrderedField n => n -> FixedSegment V2 n -> FixedSegment V2 n -> [(n, n)]
bezierClip eps p_ q_ = go p_ q_ 0 1 0 1 0 False
  where
  go p q tmin tmax umin umax clip revCurves
    | isNothing chopInterval = []

    -- split the curve if there isn't enough reduction
    | clip > 0.8 && clip' > 0.8 =
      if tmax' - tmin' > umax - umin -- split the longest segment
      then let (pl, pr) = p' `splitAtParam` 0.5
               t3 = avg tmin' tmax'
           in  go q pl umin umax tmin' t3 clip' (not revCurves) ++
               go q pr umin umax t3 tmax' clip' (not revCurves)
      else let (ql, qr) = q `splitAtParam` 0.5
               u3 = avg umin umax
           in  go ql p' umin u3 tmin' tmax' clip' (not revCurves) ++
               go qr p' u3 umax tmin' tmax' clip' (not revCurves)

    | max (umax - umin) (tmax' - tmin') < eps =
      if revCurves -- return parameters in correct order
      then [ (avg umin umax,   avg tmin' tmax') ]
      else [ (avg tmin' tmax', avg umin umax  ) ]

    -- iterate with the curves reversed.
    | otherwise = go q p' umin umax tmin' tmax' clip' (not revCurves)
    where
      chopInterval              = chopCubics p q
      Just (tminChop, tmaxChop) = chopInterval
      p'    = section p tminChop tmaxChop
      clip' = tmaxChop - tminChop
      tmin' = tmax * tminChop + tmin * (1 - tminChop)
      tmax' = tmax * tmaxChop + tmin * (1 - tmaxChop)

-- | Find the zero of a 1D bezier curve of any degree.  Note that this
--   can be used as a bernstein polynomial root solver by converting from
--   the power basis to the bernstein basis.
bezierFindRoot :: OrderedField n
               => BernsteinPoly n -- ^ the bernstein coefficients of the polynomial
               -> n   -- ^ The lower bound of the interval
               -> n   -- ^ The upper bound of the interval
               -> n   -- ^ The accuracy
               -> [n] -- ^ The roots found
bezierFindRoot p tmin tmax eps
  | isNothing chopInterval    = []
  | clip > 0.8 = let (p1, p2) = splitAtParam newP 0.5
                     tmid     = tmin' + (tmax' - tmin') / 2
                 in  bezierFindRoot p1 tmin' tmid  eps ++
                     bezierFindRoot p2 tmid  tmax' eps
  | tmax' - tmin' < eps = [avg tmin' tmax']
  | otherwise           = bezierFindRoot newP tmin' tmax' eps
  where
    chopInterval              = chopYs (bernsteinCoeffs p)
    Just (tminChop, tmaxChop) = chopInterval
    newP  = section p tminChop tmaxChop
    clip  = tmaxChop - tminChop
    tmin' = tmax * tminChop + tmin * (1 - tminChop)
    tmax' = tmax * tmaxChop + tmin * (1 - tmaxChop)

-- | Find the closest value(s) on the bezier to the given point, within tolerance.
closest :: OrderedField n => FixedSegment V2 n -> P2 n -> n -> [n]
closest cb (P (V2 px py)) = bezierFindRoot poly 0 1
  where
    (bx, by) = bezierToBernstein cb
    bx'  = bernsteinDeriv bx
    by'  = bernsteinDeriv by
    poly = (bx - listToBernstein [px, px, px, px]) * bx'
         + (by - listToBernstein [py, py, py, py]) * by'


-- | An approximation of the fat line for a cubic bezier segment. Returns 
--   @(0,0)@ for a linear segment.
fatLine :: OrderedField n => FixedSegment V2 n -> (n,n)
fatLine (FCubic p0 p1 p2 p3)
  = case (d1 > 0, d2 > 0) of
      (True,  True)  -> (0,                0.75 * max d1 d2)
      (False, False) -> (0.75 * min d1 d2, 0               )
      (True,  False) -> (4/9 * d2,         4/9 * d1        )
      (False, True)  -> (4/9 * d1,         4/9 * d2        )
  where
    d = lineDistance p0 p3
    d1 = d p1; d2 = d p2
fatLine _ = (0,0)

-- | Find the convex hull of a set of points already sorted in the x direction. 
--   The first list of the tuple is the upper hull going clockwise from 
--   left-most to right-most point. The second is the lower hull from 
--   right-most to left-most in the anti-clockwise direction.
sortedConvexHull :: OrderedField n => [P2 n] -> ([P2 n], [P2 n])
sortedConvexHull ps = (chain True ps, chain False ps)
 where
   chain upper (p1_:p2_:rest_) =
     case go (p2_ .-. p1_) p2_ rest_ of
       Right l -> p1_:l
       Left l  -> chain upper (p1_:l)
     where
       test = if upper then (>0) else (<0)
       -- find the convex hull by comparing the angles of the vectors with
       -- the cross product and backtracking if necessary.
       go dir p1 l@(p2:rest)
         -- backtrack if the direction is outward
         | test $ dir `cross2` dir' = Left l
         | otherwise                =
             case go dir' p2 rest of
               Left m  -> go dir p1 m
               Right m -> Right (p1:m)
         where
           dir' = p2 .-. p1
       go _ p1 p = Right (p1:p)

   chain _ l = l

------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------

chopYs :: OrderedField n => [n] -> Maybe (n, n)
chopYs ds = chopHull 0 0 points
  where
    points = zipWith mkP2 [fromIntegral i / fromIntegral n | i <- [0..n]] ds
    n      = length ds - 1

chopCubics :: OrderedField n => FixedSegment V2 n -> FixedSegment V2 n -> Maybe (n,n)
chopCubics p@(FCubic p0 p1 p2 p3) (FCubic q0 _ _ q3)
  = chopHull dmin dmax dps
  where
    dps = zipWith mkP2 [0, 1/3, 2/3, 1] ds
    ds  = map d [p0, p1, p2, p3]
    d   = lineDistance q0 q3
    --
    (dmin,dmax) = fatLine p
chopCubics _ _ = Nothing

-- Reduce the interval which the intersection is known to lie in using the fat 
-- line of one curve and convex hull of the points formed from the distance to 
-- the thin line of the other
chopHull :: OrderedField n => n -> n -> [P2 n] -> Maybe (n, n)
chopHull dmin dmax dps = do
  tL <- testBelow upper           $ testBetween (head upper) $ testAbove lower
  tR <- testBelow (reverse upper) $ testBetween (last upper) $ testAbove (reverse lower)
  Just (tL, tR)
    where
      (upper, lower) = sortedConvexHull dps

      testBelow (p1@(P (V2 _ y1)) : p2@(P (V2 _ y2)) : ps) continue
        | y1 >= dmin = continue
        | y1 >  y2   = Nothing
        | y2 <  dmin = testBelow (p2:ps) continue
        | otherwise  = Just $ intersectPt dmin p1 p2
      testBelow _ _  = Nothing

      testBetween (P (V2 x y)) continue
        | y <= dmax = Just x
        | otherwise = continue

      testAbove (p1@(P (V2 _ y1)) : p2@(P (V2 _ y2)) : ps)
        | y1 < y2   = Nothing
        | y2 > dmax = testAbove (p2:ps)
        | otherwise = Just $ intersectPt dmax p1 p2
      testAbove _   = Nothing

bezierToBernstein :: Fractional n => FixedSegment V2 n -> (BernsteinPoly n, BernsteinPoly n)
bezierToBernstein (FCubic a b c d) =
    (listToBernstein $ map (view _x) coeffs, listToBernstein $ map (view _y) coeffs)
  where coeffs = [a, b, c, d]
bezierToBernstein _ = error "bezierToBernstein only works on cubics"

------------------------------------------------------------------------
-- Line-Line intersection
------------------------------------------------------------------------

-- | @lineLineIntersect a b c d@ calculates the intersection between the two
--   lines defined by points @a b@ and @c d@.
lineLineIntersect :: Fractional n => P2 n -> P2 n -> P2 n -> P2 n -> P2 n
lineLineIntersect (P (V2 x1 y1)) (P (V2 x2 y2)) (P (V2 x3 y3)) (P (V2 x4 y4)) = mkP2 x y
  -- see http://mathworld.wolfram.com/Line-LineIntersection.html
  where
    x = det det1 dx1 det2 dx2 / det3
    y = det det1 dy1 det2 dy2 / det3
    --
    det1 = det x1 y1 x2 y2
    det2 = det x3 y3 x4 y4
    det3 = det dx1 dy1 dx2 dy2
    dx1  = x1 - x2
    dx2  = x3 - x4
    dy1  = y1 - y2
    dy2  = y3 - y4
    --
    det a b c d = a*d - b*c

-- | Calculate the intersecting box for two fixed segments. This is a
--   specialised version of @intersection (boundingBox a) (boundingBox b)@.
segmentIntersectingBox
  :: (Additive v, Foldable v, Fractional n, Ord n)
  => FixedSegment v n -> FixedSegment v n -> BoundingBox v n
segmentIntersectingBox (FLinear p1 p2) (FLinear p3 p4)
  = fromCorners (liftU2 max mins1 mins2) (liftU2 min maxes1 maxes2)
  where
    maxes1 = liftU2 max p1 p2
    mins1  = liftU2 min p1 p2
    maxes2 = liftU2 max p3 p4
    mins2  = liftU2 min p3 p4
segmentIntersectingBox _ _ = error "intersectingBox is only for linear segments"

