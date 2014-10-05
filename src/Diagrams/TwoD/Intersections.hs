{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE BangPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Intersections
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Paths in two dimensions are special since we may stroke them to
-- create a 2D diagram, and (eventually) perform operations such as
-- intersection and union.  They also have a trace, whereas paths in
-- higher dimensions do not.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Intersections where

import           Control.Lens hiding (contains)

import           Diagrams.Core
import           Diagrams.Parametric
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TwoD.Segment ()
import           Diagrams.TwoD.Types hiding (p2)

import           Linear.Affine
import Data.List (tails)
import Data.Maybe
import Diagrams.Prelude hiding (view, p2, p3, pr)
import Control.Monad
import Data.Foldable (Foldable)
import Diagrams.BoundingBox

------------------------------------------------------------------------
-- High level functions
------------------------------------------------------------------------

cubicIntersections :: Located (Trail V2 Double) -> Located (Trail V2 Double) -> [P2 Double]
cubicIntersections as bs = do
  a <- fixTrail as
  b <- fixTrail bs
  cubicIntersection a b

cubicIntersection :: FixedSegment V2 Double -> FixedSegment V2 Double -> [P2 Double]
cubicIntersection (FLinear _ _) _ = []
cubicIntersection _ (FLinear _ _) = []
cubicIntersection s1 s2 = map (atParam s1) ps
  where ps = map fst $ bezierIntersection s1 s2 1e-8

cross2 :: Num n => V2 n -> V2 n -> n
cross2 (V2 x1 y1) (V2 x2 y2) = x1 * y2 - y1 * x2

segmentIntersection :: RealFloat n => FixedSegment V2 n -> FixedSegment V2 n -> [P2 n]
segmentIntersection s1 s2
  = case (s1,s2) of
      (FCubic {}    , FCubic {}    ) ->
        map (atParam s1 . fst) $ segmentIntersectionParameters s1 s2

      (FCubic {}    , FLinear {}   ) ->
        map (atParam s2) $ bezierLineIntersections s1 s2 1e-4

      (FLinear {}   , FCubic {}    ) ->
        map (atParam s1) $ bezierLineIntersections s2 s1 1e-4

      (FLinear p1 p2, FLinear p3 p4) ->
        let p0  = lineLineIntersect p1 p2 p3 p4
            box = segmentIntersectingBox s1 s2

        in  guard (box `contains` p0) *> pure p0


------------------------------------------------------------------------
-- Low level functions
------------------------------------------------------------------------

-- | Calculate the parameters for each segment at which the two segments intersect.
segmentIntersectionParameters :: RealFloat n => FixedSegment V2 n -> FixedSegment V2 n -> [(n,n)]
segmentIntersectionParameters s1 s2
  = case (s1,s2) of
      (FCubic {}    , FCubic {}    ) ->
        bezierClip s1 s1 0 1 0 1 0 1e-6 False

      -- (FCubic {}    , FLinear {}   ) -> bezierLineIntersections s1 s2
      -- (FLinear {}   , FCubic {}    ) -> bezierLineIntersections s2 s1

      (FLinear p1 p2, FLinear p3 p4) ->
        let (p0, xs) = lineLineIntersectParameter p1 p2 p3 p4
            box      = segmentIntersectingBox s1 s2

        in  guard (box `contains` p0) *> pure xs

pointLineDistance :: Floating n => P2 n -> P2 n -> P2 n -> n
pointLineDistance (P (V2 x1 y1)) (P (V2 x2 y2)) (P (V2 x0 y0))
-- pointLineDistance (P (V2 x1 y1)) (P (V2 x2 y2)) (P (V2 x0 y0))
  = abs (dy * x0 - dx * y0 - x1 * y2 + x2 * y1) / sqrt (dx * dx + dy * dy)
  where
    dx = x2 - x1
    dy = y2 - y1

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

------------------------------------------------------------------------
-- Bêzier clipping algorithm
------------------------------------------------------------------------

-- find the convex hull by comparing the angles of the vectors with
-- the cross product and backtracking if necessary.
findOuter' :: OrderedField n => Bool -> V2 n -> P2 n -> [P2 n] -> Either [P2 n] [P2 n]
findOuter' !upper !dir !p1 l@(p2:rest)
  -- backtrack if the direction is outward
  | if upper
      then dir `cross2` (p2.-.p1) > 0 -- left turn
      else dir `cross2` (p2.-.p1) < 0 = Left $! l
  -- succeed
  | otherwise = case findOuter' upper (p2.-.p1) p2 rest of
    Left m  -> findOuter' upper dir p1 m
    Right m -> Right (p1:m)

findOuter' _ _ p1 p = Right $! (p1:p)

-- find the outermost point.  It doesn't look at the x values.
findOuter :: OrderedField n => Bool -> [P2 n] -> [P2 n]
findOuter upper (p1:p2:rest) =
  case findOuter' upper (p2.-.p1) p2 rest of
    Right l -> p1:l
    Left l  -> findOuter upper (p1:l)
findOuter _ l = l    

-- take the y values and turn it in into a convex hull with upper en
-- lower points separated.
makeHull :: OrderedField n => [n] -> ([P2 n], [P2 n])
makeHull ds =
  let n      = length ds - 1
      points = zipWith mkP2 [fromIntegral i/fromIntegral n | i <- [0..n]] ds
  in  (findOuter True points, findOuter False points)

-- test if the chords cross the fat line
-- return the continuation if above the line
testBelow :: OrderedField n => n -> [P2 n] -> Maybe n -> Maybe n
testBelow _    []  _ = Nothing
testBelow _    [_] _ = Nothing
testBelow !dmin (p1 : p2 : rest) cont
  | view _y p1 >= dmin       = cont
  | view _y p1 >  view _y p2 = Nothing
  | view _y p2 <  dmin       = testBelow dmin (p2:rest) cont
  | otherwise                = Just $! intersectPt dmin p1 p2

testBetween :: OrderedField n => n -> P2 n -> Maybe n -> Maybe n
testBetween !dmax (P (V2 x y)) cont
  | y <= dmax = Just x
  | otherwise = cont

-- test if the chords cross the line y=dmax somewhere
testAbove :: OrderedField n => n -> [P2 n] -> Maybe n
testAbove _    []  = Nothing
testAbove _    [_] = Nothing
testAbove dmax (p1 : p2 : rest)
  | view _y p1 < view _y p2 = Nothing
  | view _y p2 > dmax       = testAbove dmax (p2:rest)
  | otherwise               = Just $! intersectPt dmax p1 p2

-- find the x value where the line through the two points
-- intersect the line y=d
intersectPt :: OrderedField n => n -> P2 n -> P2 n -> n
intersectPt d (P (V2 x1 y1)) (P (V2 x2 y2)) =
  x1 + (d  - y1) * (x2 - x1) / (y2 - y1)

-- make a hull and test over which interval the
-- curve is garuanteed to lie inside the fat line
chopHull :: OrderedField n => n -> n -> [n] -> Maybe (n, n)
chopHull !dmin !dmax ds = do
  let (upper, lower) = makeHull ds
  left_t <- testBelow dmin upper $
            testBetween dmax (head upper) $
            testAbove dmax lower
  right_t <- testBelow dmin (reverse upper) $
             testBetween dmax (last upper) $
             testAbove dmax (reverse lower)
  Just (left_t, right_t)

bezierClip :: OrderedField n => FixedSegment V2 n -> FixedSegment V2 n -> n -> n
           -> n -> n -> n -> n -> Bool
           -> [(n, n)]
bezierClip p@(FCubic !p0 !p1 !p2 !p3) q@(FCubic !q0 !q1 !q2 !q3)
  tmin tmax umin umax prevClip eps revCurves

  -- no intersection
  | isNothing chop_interval = []

  -- not enough reduction, so split the curve in case we have
  -- multiple intersections
  | prevClip > 0.8 && newClip > 0.8 =
    if new_tmax - new_tmin > umax - umin -- split the longest segment
    then let (pl, pr) = newP `splitAtParam` 0.5
             half_t = new_tmin + (new_tmax - new_tmin) / 2
         in  bezierClip q pl umin umax new_tmin half_t newClip eps (not revCurves) ++
             bezierClip q pr umin umax half_t new_tmax newClip eps (not revCurves)
    else let (ql, qr) = q `splitAtParam` 0.5
             half_t = umin + (umax - umin) / 2
         in  bezierClip ql newP umin half_t new_tmin new_tmax newClip eps (not revCurves) ++
             bezierClip qr newP half_t umax new_tmin new_tmax newClip eps (not revCurves)

  -- within tolerance      
  | max (umax - umin) (new_tmax - new_tmin) < eps =
    if revCurves
    then [ (umin + (umax - umin)/2,
            new_tmin + (new_tmax - new_tmin)/2) ]
    else [ (new_tmin + (new_tmax - new_tmin)/2,
            umin + (umax - umin)/2) ]

  -- iterate with the curves reversed.
  | otherwise =
      bezierClip q newP umin umax new_tmin new_tmax newClip eps (not revCurves)

  where
    d = lineDistance q0 q3
    d1 = d q1
    d2 = d q2
    (dmin, dmax) | d1*d2 > 0 = (3/4 * minimum [0, d1, d2],
                                3/4 * maximum [0, d1, d2])
                 | otherwise = (4/9 * minimum [0, d1, d2],
                                4/9 * maximum [0, d1, d2])

    chop_interval = chopHull dmin dmax $ map d [p0, p1, p2, p3]
    Just (chop_tmin, chop_tmax) = chop_interval
    newP     = section p chop_tmin chop_tmax
    newClip  = chop_tmax - chop_tmin
    new_tmin = tmax * chop_tmin + tmin * (1 - chop_tmin)
    new_tmax = tmax * chop_tmax + tmin * (1 - chop_tmax)

bezierClip _ _ _ _ _ _ _ _ _ = []

-- | Find the intersections between two Bezier curves within given
-- tolerance, using the Bezier Clip algorithm. Returns the parameters
-- for both curves.
bezierIntersection :: OrderedField n => FixedSegment V2 n -> FixedSegment V2 n -> n -> [(n, n)]
bezierIntersection p q eps = bezierClip p q 0 1 0 1 0 eps False
  -- where
  --   eps' = min (bezierParamTolerance p eps) (bezierParamTolerance q eps)

------------------------------------------------------------------------
-- Bêzier-Line intersection
------------------------------------------------------------------------

------------------------ Line intersection -------------------------------------
-- Clipping a line uses a simplified version of the Bezier Clip algorithm,
-- and uses the (thin) line itself instead of the fat line.

-- | Find the zero of a 1D bezier curve of any degree.  Note that this
-- can be used as a bernstein polynomial root solver by converting from
-- the power basis to the bernstein basis.
bezierFindRoot :: OrderedField n => BernsteinPoly n -- ^ the bernstein coefficients of the polynomial
               -> n  -- ^ The lower bound of the interval 
               -> n  -- ^ The upper bound of the interval
               -> n  -- ^ The accuracy
               -> [n] -- ^ The roots found
bezierFindRoot p tmin tmax eps
  -- no intersection
  | isNothing chop_interval = []

  -- not enough reduction, so split the curve in case we have
  -- multiple intersections
  | clip > 0.8 =
    let (p1, p2) = bernsteinSplit newP 0.5
        half_t = new_tmin + (new_tmax - new_tmin) / 2
    in bezierFindRoot p1 new_tmin half_t eps ++
       bezierFindRoot p2 half_t new_tmax eps

  -- within tolerance
  | new_tmax - new_tmin < eps =
      [new_tmin + (new_tmax - new_tmin)/2]

      -- iterate
  | otherwise =
        bezierFindRoot newP new_tmin new_tmax eps

  where
    chop_interval = chopHull 0 0 (bernsteinCoeffs p)
    Just (chop_tmin, chop_tmax) = chop_interval
    newP = bernsteinSubsegment p chop_tmin chop_tmax
    clip = chop_tmax - chop_tmin
    new_tmin = tmax * chop_tmin + tmin * (1 - chop_tmin)
    new_tmax = tmax * chop_tmax + tmin * (1 - chop_tmax)

-- | Find the intersections of the curve with a line.

-- Apply a transformation to the bezier that maps the line onto the
-- X-axis.  Then we only need to test the Y-values for a zero.
bezierLineIntersections :: RealFloat n => FixedSegment V2 n -> FixedSegment V2 n -> n -> [n]
bezierLineIntersections b (FLinear p q) =
  bezierFindRoot (listToBernstein $ map (view _y) [p0, p1, p2, p3]) 0 1
  -- where (FCubic p0 p1 p2 p3) = fromJust (inverse $ translate p $* rotateVec (q ^-^ p)) $* b
  where
    (FCubic p0 p1 p2 p3) = rotate ((p .-. q) ^. _theta) . moveTo origin $ b

-- | Find the closest value(s) on the bezier to the given point, within tolerance.
closest :: OrderedField n => FixedSegment V2 n -> P2 n -> n -> [n]
closest cb (P (V2 px py)) = bezierFindRoot poly 0 1
  where
    (bx, by) = bezierToBernstein cb
    bx' = bernsteinDeriv bx
    by' = bernsteinDeriv by
    poly = (bx ~- listToBernstein [px, px, px, px]) ~* bx' ~+
           (by ~- listToBernstein [py, py, py, py]) ~* by'




bezierToBernstein :: Fractional n => FixedSegment V2 n -> (BernsteinPoly n, BernsteinPoly n)
bezierToBernstein (FCubic a b c d) = (listToBernstein $ map (view _x) coeffs, listToBernstein $ map (view _y) coeffs)
  where coeffs = [a, b, c, d]

------------------------------------------------------------------------
-- Bernstein polynomials
------------------------------------------------------------------------


data BernsteinPoly n = BernsteinPoly
  { bernsteinDegree :: !Int
  , bernsteinCoeffs :: ![n]
  } deriving Show

infixl 7 ~*, *~
infixl 6 ~+, ~-

-- | Create a bernstein polynomail from a list of coëfficients.
listToBernstein :: Num n => [n] -> BernsteinPoly n
listToBernstein [] = zeroPoly
listToBernstein l  = BernsteinPoly (length l - 1) l

-- | The constant zero.
zeroPoly :: Num n => BernsteinPoly n
zeroPoly = BernsteinPoly 0 [0]

-- | Return the subsegment between the two parameters.
bernsteinSubsegment :: OrderedField n => BernsteinPoly n -> n -> n -> BernsteinPoly n
bernsteinSubsegment b t1 t2 
  | t1 > t2   = bernsteinSubsegment b t2 t1
  | otherwise = snd $ flip bernsteinSplit (t1/t2) $
                fst $ bernsteinSplit b t2

-- multiply two bezier curves
-- control point i from the product of beziers P * Q
-- is sum (P_j * Q_k) where j + k = i+1

-- | Multiply two bernstein polynomials.  The final degree
-- will be the sum of either degrees.  This operation takes O((n+m)^2)
-- with n and m the degree of the beziers.

(~*) :: Fractional n => BernsteinPoly n -> BernsteinPoly n -> BernsteinPoly n
(BernsteinPoly la a) ~* (BernsteinPoly lb b) =
  BernsteinPoly (la+lb) $
  zipWith (flip (/)) (binCoeff (la + lb)) $
                 init $ map sum $
                 map (zipWith (*) a') (down b') ++
                 zipWith (zipWith (*)) (tail $ tails a') (repeat $ reverse b')
  where down l = tail $ scanl (flip (:)) [] l -- [[1], [2, 1], [3, 2, 1], ...
        a' = zipWith (*) a (binCoeff la)
        b' = zipWith (*) b (binCoeff lb)


-- find the binomial coefficients of degree n.
binCoeff :: Num n => Int -> [n]
binCoeff n = map fromIntegral $
             scanl (\x m -> x * (n - m+1) `quot` m) 1 [1..n]

-- | Degree elevate a bernstein polynomail a number of times.
degreeElevate :: Fractional n => BernsteinPoly n -> Int -> BernsteinPoly n
degreeElevate b 0 = b
degreeElevate (BernsteinPoly lp p) times =
  degreeElevate (BernsteinPoly (lp+1) (head p:inner p 1)) (times-1)
  where
    n = fromIntegral lp
    inner []  _ = error "empty bernstein coefficients"
    inner [a] _ = [a]
    inner (a:b:rest) i =
      (i*a/(n+1) + b*(1 - i/(n+1)))
      : inner (b:rest) (i+1)


-- | Evaluate the bernstein polynomial.
bernsteinEval :: Fractional n => BernsteinPoly n -> n -> n
bernsteinEval (BernsteinPoly _ []) _ = error "illegal bernstein polynomial"
bernsteinEval (BernsteinPoly _ [b]) _ = b
bernsteinEval (BernsteinPoly lp (b':bs)) t = go t n (b'*u) 2 bs
  where u = 1-t
        n = fromIntegral lp
        go !tn !bc !tmp _  [b] = tmp + tn*bc*b
        go !tn !bc !tmp !i (b:rest) =
          go (tn*t)         -- tn
          (bc*(n - i+1)/i)    -- bc
          ((tmp + tn*bc*b)*u) -- tmp
          (i+1)             -- i
          rest
        go _ _ _ _ [] = error "impossible"
        

-- | Evaluate the bernstein polynomial and its derivatives.
bernsteinEvalDerivs :: Fractional n => BernsteinPoly n -> n -> [n]
bernsteinEvalDerivs b t
  | bernsteinDegree b == 0 = [bernsteinEval b t]
  | otherwise = bernsteinEval b t :
                bernsteinEvalDerivs (bernsteinDeriv b) t

-- | Find the derivative of a bernstein polynomial.
bernsteinDeriv :: Num n => BernsteinPoly n -> BernsteinPoly n
bernsteinDeriv (BernsteinPoly 0 _) = zeroPoly
bernsteinDeriv (BernsteinPoly lp p) =
  BernsteinPoly (lp-1) $
  map (* fromIntegral lp) $ zipWith (-) (tail p) p

-- | Split a bernstein polynomial
bernsteinSplit :: Num n => BernsteinPoly n -> n -> (BernsteinPoly n, BernsteinPoly n)
bernsteinSplit (BernsteinPoly lp p) t =
  (BernsteinPoly lp $ map head controls,
   BernsteinPoly lp $ reverse $ map last controls)
  where
    interp a b = (1-t)*a + t*b
    terp [_] = []
    terp l = let ctrs = zipWith interp l (tail l)
             in ctrs : terp ctrs
    controls = p:terp p

-- | Sum two bernstein polynomials.  The final degree will be the maximum of either
-- degrees.
(~+) :: Fractional n => BernsteinPoly n -> BernsteinPoly n -> BernsteinPoly n
ba@(BernsteinPoly la a) ~+ bb@(BernsteinPoly lb b)
  | la < lb = BernsteinPoly lb $
              zipWith (+) (bernsteinCoeffs $ degreeElevate ba $ lb - la) b
  | la > lb = BernsteinPoly la $
              zipWith (+) a (bernsteinCoeffs $ degreeElevate bb $ la - lb)
  | otherwise = BernsteinPoly la $
                zipWith (+) a b

-- | Subtract two bernstein polynomials.  The final degree will be the maximum of either
-- degrees.
(~-) :: Fractional n => BernsteinPoly n -> BernsteinPoly n -> BernsteinPoly n
ba@(BernsteinPoly la a) ~- bb@(BernsteinPoly lb b)
  | la < lb = BernsteinPoly lb $
              zipWith (-) (bernsteinCoeffs $ degreeElevate ba (lb - la)) b
  | la > lb = BernsteinPoly la $
              zipWith (-) a (bernsteinCoeffs $ degreeElevate bb (la - lb))
  | otherwise = BernsteinPoly la $
                zipWith (-) a b

-- | Scale a bernstein polynomial by a constant.
(*~) :: Num n => n -> BernsteinPoly n -> BernsteinPoly n
a *~ (BernsteinPoly lb b) = BernsteinPoly lb (map (*a) b)


------------------------------------------------------------------------
-- Line-Line intersection
------------------------------------------------------------------------

-- | @lineLineIntersect a b c d@ calculates the intersection between the two 
--   lines defined by point @a b@ and points @c d@.
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
  :: (Foldable v, Additive v, Fractional n, Ord n)
  => FixedSegment v n -> FixedSegment v n -> BoundingBox v n
segmentIntersectingBox (FLinear p1 p2) (FLinear p3 p4)
  = fromCorners (liftU2 max mins1 mins2) (liftU2 min maxes1 maxes2)
  where
    maxes1 = liftU2 max p1 p2
    mins1  = liftU2 min p1 p2
    maxes2 = liftU2 max p3 p4
    mins2  = liftU2 min p3 p4
-- other cases for completeness
-- segmentIntersectingBox a b = intersection (boundingBox a) (boundingBox b)

lineLineIntersectParameter :: (Ord n, Fractional n) => P2 n -> P2 n -> P2 n -> P2 n -> (P2 n, (n,n))
lineLineIntersectParameter pa pb pc pd = (p0, (getParameter pa pb p0, getParameter pa pb p0))
  where
    p0 = lineLineIntersect pa pb pc pd
    getParameter (P (V2 x1 y1)) (P (V2 x2 y2)) (P (V2 x0 y0))
      | abs dx < 1e-6 = (y0 - y1) / dy
      | abs dy < 1e-6 = (x0 - x1) / dx
      | otherwise     = 0
      where
        dx = x1 - x2
        dy = y1 - y2

