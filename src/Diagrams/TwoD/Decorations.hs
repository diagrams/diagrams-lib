{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Diagrams.TwoD.Decorations
  ( -- * Rounding corners
    RoundCorners
  , roundCorners
  , roundCorners'
  , roundingCorners
  , roundingCorners'
  , roundingRadii
  ) where

import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.Decorations
import           Diagrams.Direction
import           Diagrams.Parametric
import           Diagrams.Segment
import           Diagrams.Tangent
import           Diagrams.Trail
import           Diagrams.TwoD.Arc
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector

import           Control.Lens            hiding (at)
import           Linear.Metric
import           Linear.Vector

------------------------------------------------------------------------
-- Rounded corners
------------------------------------------------------------------------

-- | Type that holds parameters for rounding corners. For now this is
--   just the radius of the arc joining the segments (not exactly true
--   for cubic segments).
newtype RoundCorners n = RoundCorners [n]
  deriving Functor

type instance V (RoundCorners n) = V2
type instance N (RoundCorners n) = n

-- | 'RoundCorners' decoration with the same rounding radius for every
--   corner.
roundCorners :: n -> RoundCorners n
roundCorners = RoundCorners . repeat

-- | 'RoundCorners' decoration with the chosen rounding radii.
roundCorners' :: [n] -> RoundCorners n
roundCorners' = RoundCorners

-- | Round the corners of something 'Morphable'.
roundingCorners :: (InSpace V2 n t, Morphable t, RealFloat n) => n -> t -> t
roundingCorners x = morph (roundCorners x)

-- | Round the corners of something 'Morphable' with a list of rounding
--   radii.
roundingCorners' :: (InSpace V2 n t, Morphable t, RealFloat n) => [n] -> t -> t
roundingCorners' xs = morph (roundCorners' xs)

-- | 'Lens'' onto the radii of a 'RoundCorners'.
roundingRadii :: Lens' (RoundCorners n) [n]
roundingRadii f (RoundCorners xs) = RoundCorners `fmap` f xs

arcBetweenSegs :: RealFloat n => n -> Segment Closed V2 n -> Segment Closed V2 n -> [Segment Closed V2 n]
arcBetweenSegs x s1 s2 =
  trailSegments r
    & scale s
    & if x<0 then map reverseSegment else id
  where
    r  = arcInOut d1 d2
    d1 = direction $ tangentAtEnd s1
    d2 = direction $ tangentAtStart s2
    -- x is the target offset of the arc
    s = x / norm (trailOffset r)

-- things that are too small to be worth drawing
small :: (Fractional n, Ord n) => n -> Bool
small = (< 1e-5) . abs

-- TODO: other ways of rounding corners
roundSegments :: RealFloat n => [n] -> [Segment Closed V2 n] -> [Segment Closed V2 n]
roundSegments = go
  where
    go (x:xs) (s1:s2:ss)
      | small x                  = s1 : go xs (s2:ss)
      | small (stdArcLength s1') = connectingArc ++ go xs (s2':ss)
      | otherwise                = s1' : connectingArc ++ go xs (s2':ss)
      where
        t1 = 1 - stdArcLengthToParam (reverseDomain s1) (abs x)
        t2 = stdArcLengthToParam s2 (abs x)

        -- I want to put limits on t1 and t2 (to stop overlap) but it's
        -- not easy in general
        (s1',s1_) = splitAtParam s1 t1
        (s2_,s2') = splitAtParam s2 t2

        -- offset arc needs to fill gap
        arcOffset     = segOffset s1_ ^+^ segOffset s2_
        connectingArc = arcBetweenSegs (signum x * norm arcOffset) s1' s2'
    go _     ss  = ss

roundSegmentsLooped :: RealFloat n => [n] -> [Segment Closed V2 n] -> [Segment Closed V2 n]
roundSegmentsLooped _ []    = []
roundSegmentsLooped xs segs = last ss : init ss
  where (s:ss) = roundSegments xs (segs ++ [s])

instance RealFloat n => Morphing (RoundCorners n) where
  morphLine (RoundCorners xs) = lineFromSegments . roundSegments xs . lineSegments
  morphLoop (RoundCorners xs) = glueLine . lineFromSegments
                              . roundSegmentsLooped xs
                              . lineSegments . cutLoop

arcInOut :: RealFloat n => Direction V2 n -> Direction V2 n -> Trail V2 n
arcInOut start end = arc (start & rotate theta) (negateRight $ angleBetweenDirs start end)
  where
    theta = negateLeft quarterTurn
    isLeft = leftTurn (fromDir start) (fromDir end)
    negateLeft  = whenever isLeft negated
    negateRight = whenever (not isLeft) negated

whenever :: Bool -> (a -> a) -> a -> a
whenever b f = if b then f else id

