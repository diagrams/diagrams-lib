{-# LANGUAGE FlexibleContexts
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Paths in two dimensions are special since we may stroke them to
-- create a 2D diagram, and (eventually) perform operations such as
-- intersection and union.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Path
       ( -- * Constructing path-based diagrams

         stroke, strokeT

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Segment
import Diagrams.Path
import Diagrams.TwoD.Types
import Diagrams.Solve

import Data.AdditiveGroup
import Data.VectorSpace
import Data.AffineSpace

import Data.Monoid
import Control.Applicative (liftA2)
import qualified Data.Foldable as F

------------------------------------------------------------
--  Constructing path-based diagrams  ----------------------
------------------------------------------------------------

-- | Convert a path into a diagram.  The resulting diagram has the
--   names 0, 1, ... assigned to each of the path's vertices.
--
--   Note that a bug in GHC 7.0.1 causes a context stack overflow when
--   inferring the type of @stroke@.  The solution is to give a type
--   signature to expressions involving @stroke@, or (recommended)
--   upgrade GHC (the bug is fixed in 7.0.2 onwards).
stroke :: (Renderable (Path R2) b)
       => Path R2 -> Diagram b R2
stroke p = mkAD (Prim p)
                (getBounds p)
                mempty
                {-  XXX what to do here?
                    fromNames $ zip ([0..] :: [Int])
                                    (pathVertices p)  -- XXX names for Bezier
                                                      --   control points too?
                -}
                (Query $ Any . flip isInsideWinding p)

-- | A composition of 'stroke' and 'pathFromTrail' for conveniently
--   converting a trail directly into a diagram.
--
--   Note that a bug in GHC 7.0.1 causes a context stack overflow when
--   inferring the type of 'stroke' and hence of @strokeT@ as well.
--   The solution is to give a type signature to expressions involving
--   @strokeT@, or (recommended) upgrade GHC (the bug is fixed in 7.0.2
--   onwards).
strokeT :: (Renderable (Path R2) b)
        => Trail R2 -> Diagram b R2
strokeT = stroke . pathFromTrail

------------------------------------------------------------
--  Inside/outside testing
------------------------------------------------------------

cross :: R2 -> R2 -> Double
cross (x,y) (x',y') = x * y' - y * x'

isInsideWinding :: P2 -> Path R2 -> Bool
isInsideWinding p = (/= 0) . crossings p

isInsideEvenOdd :: P2 -> Path R2 -> Bool
isInsideEvenOdd p = odd . crossings p

data FixedSegment v = FLinear (Point v) (Point v)
                    | FCubic (Point v) (Point v) (Point v) (Point v)
  deriving Show

mkFixedSeg :: AdditiveGroup v => Point v -> Segment v -> FixedSegment v
mkFixedSeg p (Linear v)       = FLinear p (p .+^ v)
mkFixedSeg p (Cubic c1 c2 x2) = FCubic p (p .+^ c1) (p .+^ c2) (p .+^ x2)

fAtParam :: VectorSpace v => FixedSegment v -> Scalar v -> Point v
fAtParam (FLinear p1 p2) t = alerp p1 p2 t
fAtParam (FCubic x1 c1 c2 x2) t = p3
  where p11 = alerp x1 c1 t
        p12 = alerp c1 c2 t
        p13 = alerp c2 x2 t

        p21 = alerp p11 p12 t
        p22 = alerp p12 p13 t

        p3  = alerp p21 p22 t

-- | Compute the sum of /signed/ crossings of a path as we travel in the
--   positive x direction from a given point.
crossings :: P2 -> Path R2 -> Int
crossings p = F.sum . map (trailCrossings p) . pathTrails

-- | Compute the sum of signed crossings of a trail starting from the
--   given point in the positive x direction.
trailCrossings :: P2 -> (Trail R2, P2) -> Int

  -- open trails have no inside or outside, so don't contribute crossings
trailCrossings _ (t, _) | not (isClosed t) = 0

trailCrossings p@(P (x,y)) (tr, start)
  = sum . map test
  $ zipWith mkFixedSeg (trailVertices start tr)
                       (trailSegments tr ++ [Linear . negateV . trailOffset $ tr])
  where
    test (FLinear a@(P (_,ay)) b@(P (_,by)))
      | ay <= y && by > y && isLeft a b > 0 =  1
      | by <= y && ay > y && isLeft a b < 0 = -1
      | otherwise                           =  0

    test c@(FCubic (P x1@(_,x1y)) (P c1@(_,c1y)) (P c2@(_,c2y)) (P x2@(_,x2y))) =
        sum . map testT $ ts
      where ts = filter (liftA2 (&&) (>=0) (<=1))
               $ cubForm (-  x1y + 3*c1y - 3*c2y + x2y)
                         ( 3*x1y - 6*c1y + 3*c2y)
                         (-3*x1y + 3*c1y)
                         (x1y - y)
            testT t = let (P (px,_)) = c `fAtParam` t
                      in  if px > x then signFromDerivAt t else 0
            signFromDerivAt t =
              let (dx,dy) = (3*t*t) *^ ((-1)*^x1 ^+^ 3*^c1 ^-^ 3*^c2 ^+^ x2)
                        ^+^ (2*t)   *^ (3*^x1 ^-^ 6*^c1 ^+^ 3*^c2)
                        ^+^            ((-3)*^x1 ^+^ 3*^c1)
                  ang = atan2 dy dx
              in  case () of _ | (0 < ang && ang < pi && t < 1)  -> 1
                               | (-pi < ang && ang < 0 && t > 0) -> -1
                               | otherwise                       -> 0

    isLeft a b = cross (b .-. a) (p .-. a)

