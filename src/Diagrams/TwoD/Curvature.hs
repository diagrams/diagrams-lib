{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs, ConstraintKinds            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Curvature
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Compute curvature for segments in two dimensions.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Curvature
    (
      curvature
    , radiusOfCurvature
    , squaredCurvature
    , squaredRadiusOfCurvature
    , YetMoreLikeR2
    ) where

import           Data.Monoid.Inf
import           Data.VectorSpace

import           Control.Arrow        (first, second)
import           Control.Monad        (join)

import           Diagrams.Segment
import           Diagrams.Tangent
import           Diagrams.TwoD.Types

type YetMoreLikeR2 v = (MoreLikeR2 v, Scalar (Scalar v) ~ Scalar v, RealFloat (Scalar v), VectorSpace (Scalar v))

-- | Curvature measures how curved the segment is at a point.  One intuition
-- for the concept is how much you would turn the wheel when driving a car
-- along the curve.  When the wheel is held straight there is zero curvature.
-- When turning a corner to the left we will have positive curvature.  When
-- turning to the right we will have negative curvature.
--
-- Another way to measure this idea is to find the largest circle that we can
-- push up against the curve and have it touch (locally) at exactly the point
-- and not cross the curve.  This is a tangent circle.  The radius of that
-- circle is the \"Radius of Curvature\" and it is the reciprocal of curvature.
-- Note that if the circle is on the \"left\" of the curve, we have a positive
-- radius, and if it is to the right we have a negative radius.  Straight
-- segments have an infinite radius which leads us to our representation.  We
-- result in a pair of numerator and denominator so we can include infinity and
-- zero for both the radius and the curvature.
--
--
-- Lets consider the following curve:
--
-- <<diagrams/src_Diagrams_TwoD_Curvature_diagramA.svg#diagram=diagramA&height=200&width=400>>
--
-- The curve starts with positive curvature,
--
-- <<diagrams/src_Diagrams_TwoD_Curvature_diagramPos.svg#diagram=diagramPos&height=200&width=400>>
--
-- approaches zero curvature
--
-- <<diagrams/src_Diagrams_TwoD_Curvature_diagramZero.svg#diagram=diagramZero&height=200&width=400>>
--
-- then has negative curvature
--
-- <<diagrams/src_Diagrams_TwoD_Curvature_diagramNeg.svg#diagram=diagramNeg&height=200&width=400>>
--
-- > {-# LANGUAGE GADTs #-}
-- >
-- > import Diagrams.TwoD.Curvature
-- > import Data.Monoid.Inf
-- > import Diagrams.Coordinates
-- >
-- > segmentA = Cubic (12 ^& 0) (8 ^& 10) (OffsetClosed (20 ^& 8))
-- >
-- > curveA = lw 0.1 . stroke . fromSegments $ [segmentA]
-- >
-- > diagramA = pad 1.1 . centerXY $ curveA
-- >
-- > diagramPos = diagramWithRadius 0.2
-- >
-- > diagramZero = diagramWithRadius 0.45
-- >
-- > diagramNeg = diagramWithRadius 0.8
-- >
-- > diagramWithRadius t = pad 1.1 . centerXY
-- >          $ curveA
-- >         <> showCurvature segmentA t
-- >          # withEnvelope (curveA :: D R2)
-- >          # lw 0.05 # lc red
-- >
-- > showCurvature bez@(Cubic b c (OffsetClosed d)) t
-- >   | v == 0    = mempty
-- >   | otherwise = go (radiusOfCurvature bez t)
-- >   where
-- >     v@(x,y) = unr2 $ firstDerivative b c d t
-- >     vp = (-y) ^& x
-- >
-- >     firstDerivative b c d t = let tt = t*t in (3*(3*tt-4*t+1))*^b + (3*(2-3*t)*t)*^c + (3*tt)*^d
-- >
-- >     go Infinity   = mempty
-- >     go (Finite r) = (circle (abs r) # translate vpr
-- >                  <> stroke (origin ~~ (origin .+^ vpr)))
-- >                   # moveTo (origin .+^ atParam bez t)
-- >       where
-- >         vpr = r2 (normalized vp ^* r)
-- >
--
curvature :: (YetMoreLikeR2 v)
          => Segment Closed v  -- ^ Segment to measure on.
          -> Scalar v          -- ^ Parameter to measure at.
          -> PosInf (Scalar v) -- ^ Result is a @PosInf@ value where @PosInfty@ represents
                               -- infinite curvature or zero radius of curvature.
curvature s = toPosInf . second sqrt . curvaturePair (fmap unr2 s) -- TODO: Use the generalized unr2

-- | With @squaredCurvature@ we can compute values in spaces that do not support
-- 'sqrt' and it is just as useful for relative ordering of curvatures or looking
-- for zeros.
squaredCurvature :: (YetMoreLikeR2 v) => Segment Closed v -> Scalar v -> PosInf (Scalar v)
squaredCurvature s = toPosInf . first (join (*)) . curvaturePair (fmap unr2 s) -- TODO: Use the generalized unr2


-- | Reciprocal of @curvature@.
radiusOfCurvature :: (YetMoreLikeR2 v)
                  => Segment Closed v     -- ^ Segment to measure on.
                  -> Scalar v             -- ^ Parameter to measure at.
                  -> PosInf (Scalar v)    -- ^ Result is a @PosInf@ value where @PosInfty@ represents
                                          -- infinite radius of curvature or zero curvature.
radiusOfCurvature s = toPosInf . (\(p,q) -> (q,p)) . second sqrt . curvaturePair (fmap unr2 s)

-- | Reciprocal of @squaredCurvature@
squaredRadiusOfCurvature :: (YetMoreLikeR2 v) => Segment Closed v -> Scalar v -> PosInf (Scalar v)
squaredRadiusOfCurvature s = toPosInf . (\(p,q) -> (q,p)) . first (join (*)) . curvaturePair (fmap unr2 s)


-- Package up problematic values with the appropriate infinity.
toPosInf :: RealFloat a => (a,a) -> PosInf a
toPosInf (_,0) = Infinity
toPosInf (p,q)
  | isInfinite r || isNaN r = Infinity
  | otherwise               = Finite r
  where r = p / q

-- Internal function that is not quite curvature or squaredCurvature but lets
-- us get there by either taking the square root of the numerator or squaring
-- the denominator respectively.
curvaturePair :: (Num t, Num (Scalar t), VectorSpace t)
    => Segment Closed (t, t) -> Scalar t -> (t, t)
curvaturePair (Linear _) _ = (0,1) -- Linear segments always have zero curvature (infinite radius).
curvaturePair seg@(Cubic b c (OffsetClosed d)) t = ((x'*y'' - y'*x''), (x'*x' + y'*y')^(3 :: Integer))
  where
    (x' ,y' ) = seg `tangentAtParam` t
    (x'',y'') = secondDerivative
    secondDerivative = (6*(3*t-2))*^b ^+^ (6-18*t)*^c ^+^ (6*t)*^d

-- TODO: We should be able to generalize this to higher dimensions.  See
-- <http://en.wikipedia.org/wiki/Curvature>
--
-- TODO: I'm not sure what the best way to generalize squaredCurvature to other spaces is.

