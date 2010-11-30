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
    , ellipseCoeffs
    , ellipseCenter
    , ellipseAngle
    , ellipseScale
    ) where

import "diagrams-core" Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform
import Graphics.Rendering.Diagrams.Util

import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform

import Data.Monoid (Any(..), mempty)

-- | An ellipse is represented by an affine transformation acting on
--   the unit circle.
data Ellipse = Ellipse (Transformation R2)

instance Transformable Ellipse where
  type TSpace Ellipse = R2
  transform t (Ellipse e) = Ellipse (t <> e)

-- | A unit circle.
circle :: (BSpace b ~ R2, Renderable Ellipse b) => Diagram b
circle = Diagram (prim $ Ellipse mempty)
                 (Bounds circleBounds)
                 (fromNames [ ("C", P ( 0, 0))
                            , ("E", P ( 1, 0))
                            , ("N", P ( 0, 1))
                            , ("W", P (-1, 0))
                            , ("S", P ( 0,-1)) ])
                 (\(P (x,y)) -> Any (x*x + y*y <= 1))
    where circleBounds (x,y) = 1 / sqrt(x*x + y*y)

-- | Construct an ellipse with eccentricity e, created by scaling the
--   unit circle in the Y direction.  The eccentricity must be within
--   the interval [0,1).
ellipse :: (BSpace b ~ R2, Renderable Ellipse b) => Double -> Diagram b
ellipse e
    | e >= 0 && e < 1  = scaleY (sqrt (1 - e^2)) circle
    | otherwise        = error "Eccentricity of ellipse must be >= 0 and < 1."

-- | Compute the coefficients of the quadratic form
--
--     A x^2 + B x y + C y^2 + D x + E y + F = 0
--
--   for an ellipse.  Returns A through F (in that order) in a tuple.
ellipseCoeffs :: Ellipse -> (Double, Double, Double, Double, Double, Double)
ellipseCoeffs (Ellipse eT) = (      a*a + d*d      -- x^2
                             , 2 * (a*b + d*e)     -- xy
                             ,      b*b + e*e      -- y^2
                             , 2 * (a*c + d*f)     -- x
                             , 2 * (b*c + e*f)     -- y
                             ,      c*c + f*f - 1
                             )
  where eT'   = inv eT
        (a,d) = apply eT' (1,0)
        (b,e) = apply eT' (0,1)
        (c,f) = transl eT'


-- | Compute the center of an ellipse.
ellipseCenter :: Ellipse -> P2
ellipseCenter (Ellipse e) = papply e origin

-- Below formulas taken from http://mathworld.wolfram.com/Ellipse.html

-- | Compute the angle to the major axis, measured
--   counterclockwise from the positive x axis.
ellipseAngle :: Ellipse -> Angle
ellipseAngle ell
  | b == 0 && a <= c  = 0
  | b == 0 && a >  c  = pi/2
  | a <= c            = atan (b/(a-c)) / 2
  | otherwise         = (pi + atan (b/(a-c))) / 2
  where (a,b,c,_,_,_) = ellipseCoeffs ell

-- | Compute the scaling factors of the ellipse, i.e. (a,b) where a and b are
--   half the lengths of the major and minor axes respectively.
ellipseScale :: Ellipse -> (Double, Double)
ellipseScale ell = ( sqrt (num / (r * ( disc - (a+c))))
                   , sqrt (num / (r * (-disc - (a+c))))
                   )
  where num  = 2*(a*f*f + c*d*d + g*b*b - 2*b*d*f - a*c*g)
        disc = sqrt ((a - c)^2 +4*b*b)
        r    = (b*b - a*c)
        (a,b',c,d',f',g) = ellipseCoeffs ell
        b = b'/2
        d = d'/2
        f = f'/2
