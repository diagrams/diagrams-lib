{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Path
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module defines /paths/, which are collections of concretely
-- located 'Trail's.  Many drawing systems (cairo, svg, ...) have a
-- similar notion of \"path\".  Note that paths with multiple trails
-- are necessary for being able to draw /e.g./ filled objects with
-- holes in them.
--
-----------------------------------------------------------------------------

module Diagrams.Path
       (

         -- * Paths

         Path, pathTrails

         -- * Constructing paths
         -- $construct

       , pathFromTrail
       , pathFromTrailAt
       , pathFromLocTrail

         -- * Eliminating paths

       , pathVertices
       , pathOffsets
       , pathCentroid
       , fixPath

         -- * Modifying paths

       , scalePath
       , reversePath

         -- * Miscellaneous

       , explodePath
       , partitionPath

       ) where

import           Diagrams.Align
import           Diagrams.Core
import           Diagrams.Core.Points ()
import           Diagrams.Located
import           Diagrams.Points
import           Diagrams.Segment
import           Diagrams.Trail
import           Diagrams.TrailLike
import           Diagrams.Transform

import           Control.Arrow        ((***))
import           Control.Lens         (from, makeLenses, mapped, over, view,
                                       (%~))
import           Data.AffineSpace
import qualified Data.Foldable        as F
import           Data.List            (partition)
import           Data.Semigroup
import           Data.VectorSpace

------------------------------------------------------------
--  Paths  -------------------------------------------------
------------------------------------------------------------

-- | A /path/ is a (possibly empty) list of 'Located' 'Trail's.
--   Hence, unlike trails, paths are not translationally invariant,
--   and they form a monoid under /superposition/ (placing one path on
--   top of another) rather than concatenation.
newtype Path v = Path { _pathTrails :: [Located (Trail v)] }
  deriving (Semigroup, Monoid)

makeLenses ''Path

deriving instance Show v => Show (Path v)
deriving instance Eq   v => Eq   (Path v)
deriving instance Ord  v => Ord  (Path v)

type instance V (Path v) = v

instance VectorSpace v => HasOrigin (Path v) where
  moveOriginTo = over pathTrails . map . moveOriginTo

-- | Paths are trail-like; a trail can be used to construct a
--   singleton path.
instance (InnerSpace v, OrderedField (Scalar v)) => TrailLike (Path v) where
  trailLike = Path . (:[])

-- See Note [Transforming paths]
instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
    => Transformable (Path v) where
  transform = over pathTrails . map . transform

{- ~~~~ Note [Transforming paths]

Careful!  It's tempting to just define

> transform = fmap . transform

but that doesn't take into account the fact that some
of the v's are inside Points and hence ought to be translated.
-}

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v)) => IsPrim (Path v)

instance (InnerSpace v, OrderedField (Scalar v)) => Enveloped (Path v) where
  getEnvelope = F.foldMap trailEnvelope . view pathTrails
          -- this type signature is necessary to work around an apparent bug in ghc 6.12.1
    where trailEnvelope :: Located (Trail v) -> Envelope v
          trailEnvelope (viewLoc -> (p, t)) = moveOriginTo ((-1) *. p) (getEnvelope t)

instance (InnerSpace v, OrderedField (Scalar v)) => Juxtaposable (Path v) where
  juxtapose = juxtaposeDefault

instance (InnerSpace v, OrderedField (Scalar v)) => Alignable (Path v) where
  defaultBoundary = envelopeBoundary

instance (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
    => Renderable (Path v) NullBackend where
  render _ _ = mempty

------------------------------------------------------------
--  Constructing paths  ------------------------------------
------------------------------------------------------------

-- $construct
-- Since paths are 'TrailLike', any function producing a 'TrailLike'
-- can be used to construct a (singleton) path.  The functions in this
-- section are provided for convenience.

-- | Convert a trail to a path beginning at the origin.
pathFromTrail :: (InnerSpace v, OrderedField (Scalar v)) => Trail v -> Path v
pathFromTrail = trailLike . (`at` origin)

-- | Convert a trail to a path with a particular starting point.
pathFromTrailAt :: (InnerSpace v, OrderedField (Scalar v)) => Trail v -> Point v -> Path v
pathFromTrailAt t p = trailLike (t `at` p)

-- | Convert a located trail to a singleton path.  This is equivalent
--   to 'trailLike', but provided with a more specific name and type
--   for convenience.
pathFromLocTrail :: (InnerSpace v, OrderedField (Scalar v)) => Located (Trail v) -> Path v
pathFromLocTrail = trailLike

------------------------------------------------------------
--  Eliminating paths  -------------------------------------
------------------------------------------------------------

-- | Extract the vertices of a path, resulting in a separate list of
--   vertices for each component trail (see 'trailVertices').
pathVertices :: (InnerSpace v, OrderedField (Scalar v)) => Path v -> [[Point v]]
pathVertices = map trailVertices . view pathTrails

-- | Compute the total offset of each trail comprising a path (see 'trailOffset').
pathOffsets :: (InnerSpace v, OrderedField (Scalar v)) => Path v -> [v]
pathOffsets = map (trailOffset . unLoc) . view pathTrails

-- | Compute the /centroid/ of a path (/i.e./ the average location of
--   its vertices).
pathCentroid :: (InnerSpace v, OrderedField (Scalar v)) => Path v -> Point v
pathCentroid = centroid . concat . pathVertices

-- | Convert a path into a list of lists of 'FixedSegment's.
fixPath :: (InnerSpace v, OrderedField (Scalar v)) => Path v -> [[FixedSegment v]]
fixPath = map fixTrail . view pathTrails

-- | \"Explode\" a path by exploding every component trail (see
--   'explodeTrail').
explodePath :: (VectorSpace (V t), TrailLike t) => Path (V t) -> [[t]]
explodePath = map explodeTrail . view pathTrails

-- | Partition a path into two paths based on a predicate on trails:
--   the first containing all the trails for which the predicate returns
--   @True@, and the second containing the remaining trails.
partitionPath :: (Located (Trail v) -> Bool) -> Path v -> (Path v, Path v)
partitionPath p = (view (from pathTrails) *** view (from pathTrails)) . partition p . view pathTrails

------------------------------------------------------------
--  Modifying paths  ---------------------------------------
------------------------------------------------------------

-- | Scale a path using its centroid (see 'pathCentroid') as the base
--   point for the scale.
scalePath :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v))
          => Scalar v -> Path v -> Path v
scalePath d p = (scale d `under` translation (origin .-. pathCentroid p)) p

-- | Reverse all the component trails of a path.
reversePath :: (InnerSpace v, OrderedField (Scalar v)) => Path v -> Path v
reversePath = pathTrails . mapped %~ reverseLocTrail
