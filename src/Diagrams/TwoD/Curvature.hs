{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
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
    ) where

import           Data.Monoid.Inf

import           Diagrams.Segment
import           Diagrams.Tangent
import           Diagrams.TwoD.Types
import Diagrams.Coordinates

import Linear.Vector
import Control.Lens (over)

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
-- > curveA = lw thick . stroke . fromSegments $ [segmentA]
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
-- >          # lc red
-- >
-- > showCurvature bez@(Cubic b c (OffsetClosed d)) t
-- >   | v == (0,0) = mempty
-- >   | otherwise  = go (radiusOfCurvature bez t)
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
curvature :: RealFloat n
          => Segment Closed V2 n  -- ^ Segment to measure on.
          -> n                    -- ^ Parameter to measure at.
          -> PosInf n             -- ^ Result is a @PosInf@ value where @PosInfty@ represents
                                  --   infinite curvature or zero radius of curvature.
curvature s = toPosInf . over _y sqrt . curvaturePair s
-- curvature s = toPosInf . second sqrt . curvaturePair (fmap unr2 s) -- TODO: Use the generalized unr2

-- | With @squaredCurvature@ we can compute values in spaces that do not support
-- 'sqrt' and it is just as useful for relative ordering of curvatures or looking
-- for zeros.
squaredCurvature :: RealFloat n => Segment Closed V2 n -> n -> PosInf n
-- squaredCurvature s = toPosInf . first (join (*)) . curvaturePair (fmap unr2 s) -- TODO: Use the generalized unr2
squaredCurvature s = toPosInf . over _x sq . curvaturePair s -- TODO: Use the generalized unr2


-- | Reciprocal of @curvature@.
radiusOfCurvature :: RealFloat n
                  => Segment Closed V2 n -- ^ Segment to measure on.
                  -> n                   -- ^ Parameter to measure at.
                  -> PosInf n            -- ^ Result is a @PosInf@ value where @PosInfty@ represents
                                         -- infinite radius of curvature or zero curvature.
-- radiusOfCurvature s = toPosInf . (\(p,q) -> (q,p)) . second sqrt . curvaturePair (fmap unr2 s)
radiusOfCurvature s = toPosInf . (\(V2 p q) -> V2 (sqrt q) p) . curvaturePair s

-- | Reciprocal of @squaredCurvature@
squaredRadiusOfCurvature :: RealFloat n => Segment Closed V2 n -> n -> PosInf n
-- squaredRadiusOfCurvature s = toPosInf . (\(p,q) -> (q,p)) . first (join (*)) . curvaturePair
squaredRadiusOfCurvature s = toPosInf . (\(V2 p q) -> (V2 q (sq p))) . curvaturePair s

sq :: Num a => a -> a
sq x = x * x
{-# INLINE sq #-}



-- Package up problematic values with the appropriate infinity.
toPosInf :: RealFloat a => V2 a -> PosInf a
toPosInf (V2 _ 0) = Infinity
toPosInf (V2 p q)
  | isInfinite r || isNaN r = Infinity
  | otherwise               = Finite r
  where r = p / q

-- Internal function that is not quite curvature or squaredCurvature but lets
-- us get there by either taking the square root of the numerator or squaring
-- the denominator respectively.
curvaturePair :: (Num n)
    => Segment Closed V2 n -> n -> V2 n
curvaturePair (Linear _) _                       = V2 0 1 -- Linear segments always have zero curvature (infinite radius).
curvaturePair seg@(Cubic b c (OffsetClosed d)) t = V2 (x'*y'' - y'*x'') ((x'*x' + y'*y')^(3 :: Integer))
  where
    (V2 x'  y' )     = seg `tangentAtParam` t
    (V2 x'' y'')     = secondDerivative
    secondDerivative = (6*(3*t-2))*^b ^+^ (6-18*t)*^c ^+^ (6*t)*^d

-- TODO: We should be able to generalize this to higher dimensions.  See
-- <http://en.wikipedia.org/wiki/Curvature>
--
-- TODO: I'm not sure what the best way to generalize squaredCurvature to other spaces is.

-- curvaturePair :: (Num t, Num (Scalar t), VectorSpace t)
--     => Segment Closed (t, t) -> Scalar t -> (t, t)
-- curvaturePair (Linear _) _ = (0,1) -- Linear segments always have zero curvature (infinite radius).
-- curvaturePair seg@(Cubic b c (OffsetClosed d)) t = ((x'*y'' - y'*x''), (x'*x' + y'*y')^(3 :: Integer))
--   where
--     (x' ,y' ) = seg `tangentAtParam` t
--     (x'',y'') = secondDerivative
--     secondDerivative = (6*(3*t-2))*^b ^+^ (6-18*t)*^c ^+^ (6*t)*^d
