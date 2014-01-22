{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns       #-}

module Diagrams.Project () -- export only instances
    where

import Data.AffineSpace
import Data.VectorSpace hiding (project)
import Control.Lens (under, wrapped)

import Diagrams.Core
import Diagrams.Located
import Diagrams.Parametric
import Diagrams.Path
import Diagrams.Segment
import Diagrams.Trail

-- | Cubic curves are not closed under perspective projections.
-- Therefore @Segment@s are not an instance of Projectable.  However,
-- the projection of a @Segment@ can be approximated to arbitrary
-- precision by a series of @Segment@s.  @projectSegment@ does this,
-- which allows types built from lists of @Segment@s to themselves be
-- @Projectable@.
projectSegment :: (VectorSpace v, InnerSpace v, s ~ Scalar v, Ord s, Fractional s, Floating s) =>
                  s -> Projection v -> FixedSegment v -> [FixedSegment v]
projectSegment epsilon t s
    | goodEnough epsilon t s = [approx t s]
    | otherwise  = concatMap (projectSegment epsilon t) [s1, s2]
  where
    (s1, s2) = splitAtParam s 0.5

approx :: (VectorSpace v, InnerSpace v, s ~ Scalar v, Ord s, Fractional s, Floating s) =>
          Projection v -> FixedSegment v -> FixedSegment v
approx t (FLinear p0 p1) = FLinear (project t p0) (project t p1)
approx t (FCubic p0 c1 c2 p1) = FCubic (f p0) (f c1) (f c2) (f p1) where
      f = project t

goodEnough :: (VectorSpace v, InnerSpace v, s ~ Scalar v, Ord s, Fractional s, Floating s) =>
              s -> Projection v -> FixedSegment v -> Bool
goodEnough e t s =
    all (< e) [magnitude $ project t (s `atParam` u) .-. approx t s `atParam` u
              | u <- [0.25, 0.5, 0.75]]

instance (VectorSpace v, InnerSpace v,
          s ~ Scalar v, Ord s, Fractional s, Floating s, Show s, Show v) =>
         Projectable (Located (Trail v)) where
             project' eps p t
                 | isLine $ unLoc t  = line `at` p0
                 | otherwise = glueTrail line `at` p0
               where
                 segs = concatMap (projectSegment eps p) $ fixTrail t
                 p0 = case segs of
                     (FLinear start _:_) -> start
                     (FCubic start _ _ _:_) -> start
                     _      -> loc t  -- default in case of empty trail
                 line = trailFromSegments $ map (unLoc . fromFixedSeg) segs
             project p t = project' (0.01 * extent) p t where
                           -- estimate the "size" of the Trail' as
                           -- the maximum distance to any vertex
                extent = maximum . map dist . trailVertices $ t
                dist pt = magnitude $ pt .-. loc t

instance (VectorSpace v, InnerSpace v,
          s ~ Scalar v, Ord s, Fractional s, Floating s, Show s, Show v) =>
         Projectable (Path v) where
             project' eps p = under wrapped $ map (project' eps p)
             project p = under wrapped $ map (project p)
