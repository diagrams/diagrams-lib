{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns       #-}

module Diagrams.Deform () -- export only instances
    where

import Data.AffineSpace
import Data.VectorSpace
import Control.Lens (under, wrapped)

import Diagrams.Core
import Diagrams.Located
import Diagrams.Parametric
import Diagrams.Path
import Diagrams.Segment
import Diagrams.Trail

-- | Cubic curves are not closed under perspective projections.
-- Therefore @Segment@s are not an instance of Deformable.  However,
-- the deformation of a @Segment@ can be approximated to arbitrary
-- precision by a series of @Segment@s.  @deformSegment@ does this,
-- which allows types built from lists of @Segment@s to themselves be
-- @Deformable@.
deformSegment :: (VectorSpace v, InnerSpace v, s ~ Scalar v, Ord s, Fractional s, Floating s) =>
                  s -> Deformation v -> FixedSegment v -> [FixedSegment v]
deformSegment epsilon t s
    | goodEnough epsilon t s = [approx t s]
    | otherwise  = concatMap (deformSegment epsilon t) [s1, s2]
  where
    (s1, s2) = splitAtParam s 0.5

approx :: (VectorSpace v, InnerSpace v, s ~ Scalar v, Ord s, Fractional s, Floating s) =>
          Deformation v -> FixedSegment v -> FixedSegment v
approx t (FLinear p0 p1) = FLinear (deform t p0) (deform t p1)
approx t (FCubic p0 c1 c2 p1) = FCubic (f p0) (f c1) (f c2) (f p1) where
      f = deform t

goodEnough :: (VectorSpace v, InnerSpace v, s ~ Scalar v, Ord s, Fractional s, Floating s) =>
              s -> Deformation v -> FixedSegment v -> Bool
goodEnough e t s =
    all (< e) [magnitude $ deform t (s `atParam` u) .-. approx t s `atParam` u
              | u <- [0.25, 0.5, 0.75]]

instance (VectorSpace v, InnerSpace v,
          s ~ Scalar v, Ord s, Fractional s, Floating s, Show s, Show v) =>
         Deformable (Located (Trail v)) where
             deform' eps p t
                 | isLine $ unLoc t  = line `at` p0
                 | otherwise = glueTrail line `at` p0
               where
                 segs = concatMap (deformSegment eps p) $ fixTrail t
                 p0 = case segs of
                     (FLinear start _:_) -> start
                     (FCubic start _ _ _:_) -> start
                     _      -> loc t  -- default in case of empty trail
                 line = trailFromSegments $ map (unLoc . fromFixedSeg) segs
             deform p t = deform' (0.01 * extent) p t where
                           -- estimate the "size" of the Trail' as
                           -- the maximum distance to any vertex
                extent = maximum . map dist . trailVertices $ t
                dist pt = magnitude $ pt .-. loc t

instance (VectorSpace v, InnerSpace v,
          s ~ Scalar v, Ord s, Fractional s, Floating s, Show s, Show v) =>
         Deformable (Path v) where
             deform' eps p = under wrapped $ map (deform' eps p)
             deform p = under wrapped $ map (deform p)
