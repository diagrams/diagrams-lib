{-# LANGUAGE PackageImports, FlexibleContexts, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Ellipse
-- Copyright   :  (c) Scott Walck 2010
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  walck@lvc.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Two-dimensional ellipses.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Ellipse
    ( Ellipse(..)
    , circle
    , ellipse
    , ellipseCenter
    , ellipseAngle
    , ellipseScale
    , ellipseCenterScaleAngle
    ) where

import "diagrams-core" Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Basics (prim)
import Graphics.Rendering.Diagrams.Transform

import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform

import Data.VectorSpace
import Data.LinearMap

import Data.Monoid (Any(..))

type R3 = (Double, Double, Double)
type Aug3 = (R2,Double)

type FlatFunc = R3 -> R3
type AugFunc = Aug3 -> Aug3

data Ellipse = Ellipse Double Double Double Double Double Double
               deriving (Show)
-- 6 Doubles are A, B, C, D, E, F in A x^2 + B x y + C y^2 + D x + E y + F = 0

-- Perhaps this can be done more simply, now that we have the transpose available?
instance Transformable Ellipse where
  type TSpace Ellipse = R2
  transform = undefined
  {-
  transform (Transformation t _ _) ell
      = ellipseFromFunc ((aug3Transpose tinv)
                         . (funcFromEllipse ell)
                         . tinv) where
                               tinv = lapp $ linv t
  -}

func3Transpose :: (R3 -> R3) -> (R3 -> R3)
func3Transpose func3 = \v -> (v1 `dot` v
                             ,v2 `dot` v
                             ,v3 `dot` v) where
    (v1,v2,v3) = columnsFromLinearMap func3

aug3Transpose :: (Aug3 -> Aug3) -> (Aug3 -> Aug3)
aug3Transpose = augmentFunc . func3Transpose . flattenFunc

linearMapFromColumns (v1,v2,v3) = linear $ \(x,y,z) -> (x*^v1,y*^v2,z*^v3)

columnsFromLinearMap lmap = (v1,v2,v3) where
    v1 = lmap (1,0,0)
    v2 = lmap (0,1,0)
    v3 = lmap (0,0,1)

aug :: R3 -> Aug3
aug (a,b,c) = ((a,b),c)

unAug :: Aug3 -> R3
unAug ((a,b),c) = (a,b,c)

augmentFunc :: (R3 -> R3) -> (Aug3 -> Aug3)
augmentFunc f = aug . f . unAug

flattenFunc :: (Aug3 -> Aug3) -> (R3 -> R3)
flattenFunc g = unAug . g . aug

cross :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
cross (ax,ay,az) (bx,by,bz) = (ay * bz - az * by,az * bx - ax * bz,ax * by - ay * bx)

dot :: (Double,Double,Double) -> (Double,Double,Double) -> Double
dot (ax,ay,az) (bx,by,bz) = ax * bx + ay * by + az * bz

funcFromEllipse :: Ellipse -> AugFunc
funcFromEllipse (Ellipse a b c d e f)
    = \((x,y),z) -> ((a   * x + b/2 * y + d/2 * z
                     ,b/2 * x + c   * y + e/2 * z)
                    ,d/2 * x + e/2 * y + f   * z)

ellipseFromFunc :: AugFunc -> Ellipse
ellipseFromFunc func
    = Ellipse a b c d e f where
      ((a     ,halfb1),halfd1) = func ((1,0),0)
      ((halfb2,c     ),halfe1) = func ((0,1),0)
      ((halfd2,halfe2),f     ) = func ((0,0),1)
      b = halfb1 + halfb2
      d = halfd1 + halfd2
      e = halfe1 + halfe2

-- | A unit circle
circle :: (BSpace b ~ R2, Renderable Ellipse b) => Diagram b
circle = Diagram (prim $ Ellipse 1 0 1 0 0 (-1))
                 (Bounds circleBounds)
                 (fromNames [ ("C", P ( 0, 0))
                            , ("E", P ( 1, 0))
                            , ("N", P ( 0, 1))
                            , ("W", P (-1, 0))
                            , ("S", P ( 0,-1)) ])
                 (\(P (x,y)) -> Any (x*x + y*y <= 1))
    where circleBounds (x,y) = 1 / sqrt(x*x + y*y)

-- | Ellipse with eccentricity e
ellipse :: (BSpace b ~ R2, Renderable Ellipse b) => Double -> Diagram b
ellipse e
    | e >= 0 && e < 1  = scaleY (sqrt (1 - e^2)) circle
    | otherwise        = error "Eccentricity of ellipse must be >= 0 and < 1."

-- | Returns (xCenter,yCenter)
ellipseCenter :: Ellipse -> P2
ellipseCenter (Ellipse a b c d e f)
    = P $ (2*c*d-b*e,2*a*e-b*d) ^/ (b*b - 4*a*c)

-- | Returns long axis angle, measured counterclockwise from x axis toward y axis
ellipseAngle :: Ellipse -> Angle
ellipseAngle (Ellipse a b c d e f)
    = atan2 (sqrt((c - a)^2 + b^2) + c - a) b

-- | Returns (xScale,yScale)
ellipseScale :: Ellipse -> R2
ellipseScale (Ellipse a b c d e f)
    = (1/sqrt(lam1),1/sqrt(lam2)) where
      lam1 = (a + c + sqrt((a - c)^2 + b^2))/(-2 * f)
      lam2 = (a + c - sqrt((a - c)^2 + b^2))/(-2 * f)

-- | Returns (xCenter,yCenter,xScale,yScale,angle)
ellipseCenterScaleAngle :: Ellipse -> (Double,Double,Double,Double,Double)
ellipseCenterScaleAngle (Ellipse a b c d e f)
    = (xc,yc,xs,ys,th) where  -- xc = x center, ys = y scale, th = angle
      (xc,yc) = (2*c*d-b*e,2*a*e-b*d) ^/ (b*b - 4*a*c)
      th = atan2 (sqrt((c - a)^2 + b^2) + c - a) b
      (xs,ys) = (1/sqrt(lam1),1/sqrt(lam2))
      lam1 = (a + c + sqrt((a - c)^2 + b^2))/(-2 * f')
      lam2 = (a + c - sqrt((a - c)^2 + b^2))/(-2 * f')
      f' = f - (a * xc^2 + b * xc * yc + c * yc^2)

