{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Orphan Traced instances for Segment Closed V2 and FixedSegment V2.
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
  ( -- * Segment intersections

    intersectPointsS
  , intersectPointsS'

    -- * Closest point on a segment

  , closestPoint
  , closestPoint'
  , closestDistance
  , closestDistance'
  , closestParam
  , closestParam'

    -- ** Low level functions
  , segmentSegment
  , lineSegment
  )
  where

import           Control.Lens                    hiding (at, contains, transform, ( # ))
import           Data.Maybe

import           Diagrams.Core

import           Diagrams.Direction
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Segment
import           Diagrams.TwoD.Points
import           Diagrams.TwoD.Segment.Bernstein
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types             hiding (p2)
import           Diagrams.TwoD.Vector

import           Linear.Affine
import           Linear.Metric

{- All instances of Traced should maintain the invariant that the list of
   traces is sorted in increasing order.
-}

instance OrderedField n => Traced (Segment Closed V2 n) where
  getTrace = getTrace . mkFixedSeg . (`at` origin)

instance OrderedField n => Traced (FixedSegment V2 n) where
  getTrace seg = mkTrace $ \p v ->
    mkSortedList . map (view _1) $ lineSegment defEps (v `at` p) seg

defEps :: Fractional n => n
defEps = 1e-8

-- | Compute the intersections between two fixed segments.
intersectPointsS :: OrderedField n => FixedSegment V2 n -> FixedSegment V2 n -> [P2 n]
intersectPointsS = intersectPointsS' defEps

-- | Compute the intersections between two segments using the given tolerance.
intersectPointsS' :: OrderedField n => n -> FixedSegment V2 n -> FixedSegment V2 n -> [P2 n]
intersectPointsS' eps s1 s2 = map (view _3) $ segmentSegment eps s1 s2

-- | Get the closest distance(s) from a point to a 'FixedSegment'.
closestDistance :: OrderedField n => FixedSegment V2 n -> P2 n -> [n]
closestDistance = closestDistance' defEps

-- | Get the closest distance(s) from a point to a 'FixedSegment' within given
--   tolerance.
closestDistance' :: OrderedField n => n -> FixedSegment V2 n -> P2 n -> [n]
closestDistance' eps seg p = map (distanceA p) $ closestPoint' eps seg p

-- | Get the closest point(s) on a 'FixedSegment' from a point.
closestPoint :: OrderedField n => FixedSegment V2 n -> P2 n -> [P2 n]
closestPoint = closestPoint' defEps

-- | Get the closest point(s) on a 'FixedSegment' from a point within given
--   tolerance.
closestPoint' :: OrderedField n => n -> FixedSegment V2 n -> P2 n -> [P2 n]
closestPoint' eps seg = map (seg `atParam`) . closestParam' eps seg

-- | Find the closest value(s) on the Bêzier to the given point.
closestParam :: OrderedField n => FixedSegment V2 n -> P2 n -> [n]
closestParam = closestParam' defEps

-- | Find the closest value(s) on the Bêzier to the given point within given
--   tolerance.
closestParam' :: OrderedField n => n -> FixedSegment V2 n -> P2 n -> [n]
closestParam' _ (FLinear p0 p1) p
  | t < 0     = [0]
  | t > 1     = [1]
  | otherwise = [t]
  where
    vp = p  .-. p0
    v  = p1 .-. p0
    dp = vp `dot` v
    t  = dp / quadrance v
closestParam' eps cb (P (V2 px py)) = bezierFindRoot eps poly 0 1
  where
    (bx, by) = bezierToBernstein cb
    bx'  = bernsteinDeriv bx
    by'  = bernsteinDeriv by
    poly = (bx - listToBernstein [px, px, px, px]) * bx'
         + (by - listToBernstein [py, py, py, py]) * by'

------------------------------------------------------------------------
-- Low level
------------------------------------------------------------------------

-- | Return the intersection points with the parameters at which each segment
--   intersects.
segmentSegment :: OrderedField n => n -> FixedSegment V2 n -> FixedSegment V2 n -> [(n, n, P2 n)]
segmentSegment eps s1 s2 =
  case (s1,s2) of
    (FCubic{}, FCubic{})  -> map (\(t1,t2) -> (t1,t2, s1 `atParam` t1))
                           $ bezierClip eps s1 s2
    (FCubic{}, FLinear{}) -> map flip12 $ linearSeg (segLine s2) s1
    _                     -> linearSeg (segLine s1) s2 -- s1 is linear
  where
    linearSeg l s  = filter (inRange . view _1) $ lineSegment eps l s
    flip12 (a,b,c) = (b,a,c)

-- | Return the intersection points with the parameters at which the line and segment
--   intersect.
lineSegment :: OrderedField n => n -> Located (V2 n) -> FixedSegment V2 n -> [(n, n, P2 n)]
lineSegment _ l1 p@(FLinear p0 p1)
  = map (\(tl,tp) -> (tl, tp, p `atParam` tp))
  . filter (inRange . snd) . maybeToList $ lineLine l1 (mkLine p0 p1)
lineSegment eps (viewLoc -> (p,r)) cb = map addPoint params
  where
    params = bezierFindRoot eps (listToBernstein $ cb' ^.. each . _y) 0 1
    cb'    = transform (inv (rotationTo $ dir r)) . moveOriginTo p $ cb
    --
    addPoint bt = (lt, bt, intersect)
      where
        intersect = cb `atParam` bt
        lt        = (cb' `atParam` bt) ^. _x / norm r

-- Adapted from from kuribas's cubicbezier package https://github.com/kuribas/cubicbezier

-- | Use the Bêzier clipping algorithm to return the parameters at which the
--   Bêzier curves intersect.
bezierClip :: OrderedField n => n -> FixedSegment V2 n -> FixedSegment V2 n -> [(n, n)]
bezierClip eps p_ q_ = filter (allOf both inRange) -- sometimes this returns NaN
                     $ go p_ q_ 0 1 0 1 0 False
  where
  go p q tmin tmax umin umax clip revCurves
    | isNothing chopInterval = []

    -- This check happens before the subdivision
    -- test to avoid non-termination as values
    -- transition to within epsilon.
    | max (umax - umin) (tmax' - tmin') < eps =
      if revCurves -- return parameters in correct order
      then [ (avg umin  umax,  avg tmin' tmax') ]
      else [ (avg tmin' tmax', avg umin  umax ) ]

    -- split the curve if there isn't enough reduction
    | clip > 0.8 && clip' > 0.8 =
      if tmax' - tmin' > umax - umin -- split the longest segment
      then let (pl, pr) = p' `splitAtParam` 0.5
               tmid = avg tmin' tmax'
           in  go q pl umin umax tmin' tmid  clip' (not revCurves) ++
               go q pr umin umax tmid  tmax' clip' (not revCurves)
      else let (ql, qr) = q `splitAtParam` 0.5
               umid = avg umin umax
           in  go ql p' umin umid tmin' tmax' clip' (not revCurves) ++
               go qr p' umid umax tmin' tmax' clip' (not revCurves)

    -- iterate with the curves reversed.
    | otherwise = go q p' umin umax tmin' tmax' clip' (not revCurves)
    where
      chopInterval              = chopCubics p q
      Just (tminChop, tmaxChop) = chopInterval
      p'    = section p tminChop tmaxChop
      clip' = tmaxChop - tminChop
      tmin' = tmax * tminChop + tmin * (1 - tminChop)
      tmax' = tmax * tmaxChop + tmin * (1 - tmaxChop)

-- | Find the zero of a 1D Bêzier curve of any degree.  Note that this
--   can be used as a Bernstein polynomial root solver by converting from
--   the power basis to the Bernstein basis.
bezierFindRoot :: OrderedField n
               => n   -- ^ The accuracy
               -> BernsteinPoly n -- ^ the Bernstein coefficients of the polynomial
               -> n   -- ^ The lower bound of the interval
               -> n   -- ^ The upper bound of the interval
               -> [n] -- ^ The roots found
bezierFindRoot eps p tmin tmax
  -- If we generated the max number of roots and tmax is also a root
  -- (which is not among the generated ones), there must in fact be an
  -- infinite number of roots, so just include tmax.
  | length roots == bernsteinDegree p && last roots /= tmax && abs (evaluateBernstein p tmax) <= eps
  = roots ++ [tmax]
  | otherwise = roots
 where
  -- Lazily take a number of roots at most the degree of the bernstein
  -- polynomial, to avoid generating a ton of roots in the case of a
  -- straight Bezier segment along the x-axis. See https://github.com/diagrams/diagrams-contrib/issues/91 .
  roots = take (bernsteinDegree p) $ go p tmin tmax
  go p tmin tmax
    | isNothing chopInterval = []
    | tmax' - tmin' < eps = [avg tmin' tmax']
    | clip > 0.8 = let (p1, p2) = splitAtParam newP 0.5
                       tmid     = tmin' + (tmax' - tmin') / 2
                   in  go p1 tmin' tmid  ++
                       go p2 tmid  tmax'
    | otherwise = bezierFindRoot eps newP tmin' tmax'
    where
      chopInterval              = chopYs (bernsteinCoeffs p)
      Just (tminChop, tmaxChop) = chopInterval
      newP  = section p tminChop tmaxChop
      clip  = tmaxChop - tminChop
      tmin' = tmax * tminChop + tmin * (1 - tminChop)
      tmax' = tmax * tmaxChop + tmin * (1 - tmaxChop)

------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------

-- | An approximation of the fat line for a cubic Bêzier segment. Returns
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

chopYs :: OrderedField n => [n] -> Maybe (n, n)
chopYs ds = chopHull 0 0 points
  where
    points = zipWith mkP2 [fromIntegral i / fromIntegral n | i <- [0..n]] ds
    n      = length ds - 1

chopCubics :: OrderedField n => FixedSegment V2 n -> FixedSegment V2 n -> Maybe (n,n)
chopCubics p q@(FCubic q0 _ _ q3)
  = chopHull dmin dmax dps
  where
    dps = zipWith mkP2 [0, 1/3, 2/3, 1] ds
    ds  = p ^.. each . to d
    d   = lineDistance q0 q3
    --
    (dmin,dmax) = fatLine q
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
        | y1 < y2      = Nothing
        | y2 > dmax    = testAbove (p2:ps)
        | y2 - y1 == 0 = Nothing  -- Check this condition to prevent
                                  -- division by zero in `intersectPt`.
        | otherwise    = Just $ intersectPt dmax p1 p2
      testAbove _      = Nothing

      -- find the x value where the line through the two points
      -- intersect the line y=d.  Note that `y2 - y1 != 0` due
      -- to checks above.
      intersectPt d (P (V2 x1 y1)) (P (V2 x2 y2)) =
          x1 + (d - y1) * (x2 - x1) / (y2 - y1)



bezierToBernstein :: Fractional n => FixedSegment V2 n -> (BernsteinPoly n, BernsteinPoly n)
bezierToBernstein seg =
    (listToBernstein $ map (view _x) coeffs, listToBernstein $ map (view _y) coeffs)
  where coeffs = toListOf each seg

------------------------------------------------------------------------
-- Lines
------------------------------------------------------------------------

-- Could split this into a separate module.

-- | Returns @(a, b, c, d)@ such that @ax + by + c = 0@ is the line going through
--   @p1@ and @p2@ with @(a^2)/d + (b^2)/d = 1@.  We delay the division by
--   @d@ as it may not be needed in all cases and @d@ may be zero.
lineEquation :: Floating n => P2 n -> P2 n -> (n, n, n, n)
lineEquation (P (V2 x1 y1)) (P (V2 x2 y2)) = (a, b, c, d)
  where
    c  = -(x1*a + y1*b)
    a = y1 - y2
    b = x2 - x1
    d  = a*a + b*b

-- | Return the distance from a point to the line.
lineDistance :: (Ord n, Floating n) => P2 n -> P2 n -> P2 n -> n
lineDistance p1 p2 p3@(P (V2 x y))
    -- I have included the check that d' <= 0 in case
    -- there exists some d > 0 where sqrt d == 0.  I don't
    -- think this can happen as sqrt is at least recommended
    -- to be within one value of correct for sqrt and near
    -- zero values get bigger.
    | d <= 0 || d' <= 0 = norm (p1 .-. p3)
    | otherwise = (a*x + b*y + c) / d'
  where
    (a, b, c, d) = lineEquation p1 p2
    d' = sqrt d

-- clockwise :: (Num n, Ord n) => V2 n -> V2 n -> Bool
-- clockwise a b = a `cross2` b <= 0

avg :: Fractional n => n -> n -> n
avg a b = (a + b)/2

lineLine :: (Fractional n, Eq n) => Located (V2 n) -> Located (V2 n) -> Maybe (n,n)
lineLine (viewLoc -> (p,r)) (viewLoc -> (q,s))
  | x1 == 0 && x2 /= 0 = Nothing                 -- parallel
  | otherwise          = Just (x3 / x1, x2 / x1) -- intersecting or collinear
  where
    x1 = r × s
    x2 = v × r
    x3 = v × s
    v  = q .-. p

(×) :: Num n => V2 n -> V2 n -> n
(×) = cross2

mkLine :: InSpace v n (v n) => Point v n -> Point v n -> Located (v n)
mkLine p0 p1 = (p1 .-. p0) `at` p0

segLine :: InSpace v n (v n) => FixedSegment v n -> Located (v n)
segLine (FLinear p0 p1)    = mkLine p0 p1
segLine (FCubic p0 _ _ p3) = mkLine p0 p3

-- This function uses `defEps`, but is used in functions
-- above that take an epsilon parameter.  It would be nice
-- to clearify the meaning of each of these epsilons.
inRange :: (Fractional n, Ord n) => n -> Bool
inRange x = x < (1+defEps) && x > (-defEps)

