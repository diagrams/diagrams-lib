{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Decorations
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- There are two main kinds of \"decorations\". The first are called
-- \"Morphs\". These 'morph' trail-like objects, preserving whether
-- the 'Trail' is a 'Line' or a 'Loop'. These are useful because they can
-- be implemented by the 'Morphing' class to work in many different
-- types.
--
-- The other kind of decorations take a path and return whatever they
-- like (usually a 'Diagram' or 'Path') using that path.
--
-----------------------------------------------------------------------------
module Diagrams.Decorations
  ( -- * Morphing class
    Morphing (..)
  , morphTrail
  , morphLocLine
  , morphLocLoop
  , morphLocTrail
  , morphPath
  , morphToPath

  -- * Morphable class
  , Morphable (..)

  -- * Morph type
  , TrailMorph (..)
  , mkMorph
  , fromMorphing
  ) where

import           Data.Semigroup
import           Control.Lens     hiding (at)

import           Diagrams.Core
import           Diagrams.Located
import           Diagrams.Path
import           Diagrams.Trail

import           Linear.Vector
import           Linear.Metric

------------------------------------------------------------------------
-- Decoration class
------------------------------------------------------------------------

-- | Class of things that can be used to morph a trail while preserving
--   its type (keeping 'Line's 'Line's and 'Loop's 'Loop's).
class Morphing d where
  morphLine :: d -> Trail' Line (V d) (N d) -> Trail' Line (V d) (N d)
  morphLoop :: d -> Trail' Loop (V d) (N d) -> Trail' Loop (V d) (N d)

morphTrail :: (InSpace v n d, Morphing d) => d -> Trail v n -> Trail v n
morphTrail d = onTrail (morphLine d) (morphLoop d)

morphLocLine :: (InSpace v n d, Morphing d)
           => d -> Located (Trail' Line v n) -> Located (Trail' Line v n)
morphLocLine d = mapLoc (morphLine d)

morphLocLoop :: (InSpace v n d, Morphing d)
           => d -> Located (Trail' Loop v n) -> Located (Trail' Loop v n)
morphLocLoop d = mapLoc (morphLoop d)

morphLocTrail :: (InSpace v n d, Morphing d)
            => d -> Located (Trail v n) -> Located (Trail v n)
morphLocTrail d = mapLoc (morphTrail d)

morphPath :: (InSpace v n d, Morphing d) => d -> Path v n -> Path v n
morphPath d = each %~ morphLocTrail d

morphToPath :: (InSpace v n d, SameSpace d t, Morphing d, ToPath t, Metric v, OrderedField n) => d -> t -> Path v n
morphToPath d = morphPath d . toPath

-------------------
-- Morphable class
-------------------

-- | Class of things that can be decorated while preserving type.
class Morphable t where
  morph :: (SameSpace d t, Morphing d) => d -> t -> t

instance (Additive v, Num n) => Morphable (Trail' l v n) where
  morph d t@(Line _)   = morphLine d t
  morph d t@(Loop _ _) = morphLoop d t

instance (Additive v, Num n) => Morphable (Trail v n) where
  morph = morphTrail

instance Morphable t => Morphable (Located t) where
  morph d = mapLoc (morph d)

instance (Additive v, Num n) => Morphable (Path v n) where
  morph = morphPath

------------------
-- Morphable type
------------------

-- | General decoration type. Useful for composing decorations.
data TrailMorph v n =
  M (Trail' Line v n -> Trail' Line v n)
    (Trail' Loop v n -> Trail' Loop v n)

type instance V (TrailMorph v n) = v
type instance N (TrailMorph v n) = n

instance Semigroup (TrailMorph v n) where
  M f1 g1 <> M f2 g2 = M (f1 . f2) (g1 . g2)

instance Monoid (TrailMorph v n) where
  mappend = (<>)
  mempty  = M id id

instance InSpace v n (v n) => Morphing (TrailMorph v n) where
  morphLine (M f _) = f
  morphLoop (M _ g) = g

mkMorph :: (Trail' Line v n -> Trail' Line v n) -> (Trail' Loop v n -> Trail' Loop v n) -> TrailMorph v n
mkMorph = M

fromMorphing :: (InSpace v n d, Morphing d) => d -> TrailMorph v n
fromMorphing d = M (morphLine d) (morphLoop d)

------------------------------------------------------------------------
-- Decorations
------------------------------------------------------------------------

-- todo

