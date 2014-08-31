{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Diagrams.Deform
       ( Deformation(..)
       , Deformable(..)
       , asDeformation
       ) where

import           Control.Lens        (under, _Unwrapped)
import           Data.Monoid         hiding ((<>))
import           Data.Semigroup

import           Diagrams.Core
import           Diagrams.Located
import           Diagrams.Parametric
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Trail

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

------------------------------------------------------------
-- Deformations

-- | @Deformations@ are a superset of the affine transformations
-- represented by the 'Transformation' type.  In general they are not
-- invertable.  @Deformation@s include projective transformations.
-- @Deformation@ can represent other functions from points to points
-- which are "well-behaved", in that they do not introduce small wiggles.
data Deformation v n = Deformation (Point v n -> Point v n)

instance Semigroup (Deformation v n) where
  (Deformation p1) <> (Deformation p2) = Deformation (p1 . p2)

instance Monoid (Deformation v n) where
  mappend = (<>)
  mempty = Deformation id

class Deformable a where
  -- | @deform' epsilon d a@ transforms @a@ by the deformation @d@.
  -- If the type of @a@ is not closed under projection, approximate
  -- to accuracy @epsilon@.
  deform' :: Vn a ~ v n => n -> Deformation v n -> a -> a

  -- | @deform d a@ transforms @a@ by the deformation @d@.
  -- If the type of @a@ is not closed under projection, @deform@
  -- should call @deform'@ with some reasonable default value of
  -- @epsilon@.
  deform :: Deformation (V a) (N a) -> a -> a

-- | @asDeformation@ converts a 'Transformation' to a 'Deformation' by
-- discarding the inverse transform.  This allows reusing
-- @Transformation@s in the construction of @Deformation@s.
asDeformation :: (Additive v, Num n) => Transformation v n -> Deformation v n
asDeformation t = Deformation f'
  where
    f' = papply t

------------------------------------------------------------
-- Instances

instance Deformable (Point v n) where
  deform' = const deform

  deform (Deformation l) = l

-- | Cubic curves are not closed under perspective projections.
-- Therefore @Segment@s are not an instance of Deformable.  However,
-- the deformation of a @Segment@ can be approximated to arbitrary
-- precision by a series of @Segment@s.  @deformSegment@ does this,
-- which allows types built from lists of @Segment@s to themselves be
-- @Deformable@.
deformSegment :: (Metric v, OrderedField n)
   => n -> Deformation v n -> FixedSegment v n -> [FixedSegment v n]
deformSegment epsilon t s
    | goodEnough epsilon t s = [approx t s]
    | otherwise              = concatMap (deformSegment epsilon t) [s1, s2]
  where
    (s1, s2) = splitAtParam s 0.5

approx :: (Metric v, OrderedField n)
  => Deformation v n -> FixedSegment v n -> FixedSegment v n
approx t (FLinear p0 p1) = FLinear (deform t p0) (deform t p1)
approx t (FCubic p0 c1 c2 p1) = FCubic (f p0) (f c1) (f c2) (f p1) where
      f = deform t

goodEnough :: (Metric v, Ord n, Floating n)
  => n -> Deformation v n -> FixedSegment v n -> Bool
goodEnough e t s =
    all (< e) [norm $ deform t (s `atParam` u) .-. approx t s `atParam` u
              | u <- [0.25, 0.5, 0.75]]

instance (Metric v, OrderedField n) => Deformable (Located (Trail v n)) where
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

  deform p t = deform' (0.01 * extent) p t
    where
      -- estimate the "size" of the Trail' as
      -- the maximum distance to any vertex
      extent = maximum . map dist . trailVertices $ t
      dist pt = norm $ pt .-. loc t

instance (Metric v, OrderedField n) => Deformable (Path v n) where
  deform' eps p = under _Unwrapped $ map (deform' eps p)
  deform p      = under _Unwrapped $ map (deform p)

