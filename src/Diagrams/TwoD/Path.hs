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

       , isInsideWinding, isInsideEvenOdd
       , crossings, trailCrossings
       , mkFixedSeg

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Segment
import Diagrams.Path
import Diagrams.TwoD.Types

import Data.AdditiveGroup
import Data.AffineSpace

import Data.Monoid
import qualified Data.Set as S
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
                mempty   -- Paths are infinitely thin
                         -- TODO: what about closed paths in 2D?

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

isInsideWinding p = (/= 0) . crossings p
isInsideEvenOdd p = odd . crossings p

data FixedSegment v = FLinear (Point v) (Point v)
                    | FCubic (Point v) (Point v) (Point v) (Point v)
  deriving Show

mkFixedSeg :: AdditiveGroup v => Point v -> Segment v -> FixedSegment v
mkFixedSeg p (Linear v)       = FLinear p (p .+^ v)
mkFixedSeg p (Cubic c1 c2 x2) = FCubic p (p .+^ c1) (p .+^ c2) (p .+^ x2)

-- | Compute the sum of /signed/ crossings of a path as we travel in the
--   positive x direction from a given point.
crossings :: P2 -> Path R2 -> Int
crossings p = F.sum . S.map (trailCrossings p) . pathTrails

-- | Compute the sum of signed crossings of a trail starting from the
--   given point in the positive x direction.
trailCrossings :: P2 -> (Trail R2, P2) -> Int

  -- open trails have no inside or outside, so don't contribute crossings
trailCrossings _ (t, _) | not (isClosed t) = 0

trailCrossings p@(P (_,y)) (t, start)
  = sum . map test
  $ zipWith mkFixedSeg (trailVertices start t)
                       (trailSegments t ++ [Linear . negateV . trailOffset $ t])
  where
    test l@(FLinear (P (_,ay)) (P (_,by)))
      | ay <= y = if by <= y then isLeft l else 0
      | by <= y = negate (isLeft l)
      | otherwise = 0

    isLeft (FLinear a b) = floor $ signum (cross (b .-. a) (p .-. a))

