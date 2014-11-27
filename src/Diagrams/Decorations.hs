{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Diagrams.Decorations
  ( -- * Decorating class
    Decorating (..)
  , decTrail
  , decLocLine
  , decLocLoop
  , decLocTrail
  , decPath
  , decToPath

  -- * Decoratable class
  , Decorateable (..)

  -- * Decoration type
  , Decoration (..)
  , mkDecoration
  , fromDecorating
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

-- | Class of things that can be used to decorate a trail while
--   preserving its type.
class Decorating d where
  decLine :: d -> Trail' Line (V d) (N d) -> Trail' Line (V d) (N d)
  decLoop :: d -> Trail' Loop (V d) (N d) -> Trail' Loop (V d) (N d)

decTrail :: (InSpace v n d, Decorating d) => d -> Trail v n -> Trail v n
decTrail d = onTrail (decLine d) (decLoop d)

decLocLine :: (InSpace v n d, Decorating d)
           => d -> Located (Trail' Line v n) -> Located (Trail' Line v n)
decLocLine d = mapLoc (decLine d)

decLocLoop :: (InSpace v n d, Decorating d)
           => d -> Located (Trail' Loop v n) -> Located (Trail' Loop v n)
decLocLoop d = mapLoc (decLoop d)

decLocTrail :: (InSpace v n d, Decorating d)
            => d -> Located (Trail v n) -> Located (Trail v n)
decLocTrail d = mapLoc (decTrail d)

decPath :: (InSpace v n d, Decorating d) => d -> Path v n -> Path v n
decPath d = each %~ decLocTrail d

decToPath :: (InSpace v n d, SameSpace d t, Decorating d, ToPath t, Metric v, OrderedField n) => d -> t -> Path v n
decToPath d = decPath d . toPath

------------------------------------------------------------------------
-- Decorateable class
------------------------------------------------------------------------

-- | Class of things that can be decorated while preserving type.
class Decorateable t where
  decorate :: (SameSpace d t, Decorating d) => d -> t -> t

instance (Additive v, Num n) => Decorateable (Trail' l v n) where
  decorate d t@(Line _)   = decLine d t
  decorate d t@(Loop _ _) = decLoop d t

instance (Additive v, Num n) => Decorateable (Trail v n) where
  decorate = decTrail

instance Decorateable t => Decorateable (Located t) where
  decorate d = mapLoc (decorate d)

instance (Additive v, Num n) => Decorateable (Path v n) where
  decorate = decPath

------------------------------------------------------------------------
-- Decorateable type
------------------------------------------------------------------------

-- | General decoration type. Useful for composing decorations.
data Decoration v n =
  D (Trail' Line v n -> Trail' Line v n)
    (Trail' Loop v n -> Trail' Loop v n)

type instance V (Decoration v n) = v
type instance N (Decoration v n) = n

instance Semigroup (Decoration v n) where
  D f1 g1 <> D f2 g2 = D (f1 . f2) (g1 . g2)

instance Monoid (Decoration v n) where
  mappend = (<>)
  mempty  = D id id

instance InSpace v n (v n) => Decorating (Decoration v n) where
  decLine (D f _) = f
  decLoop (D _ g) = g

mkDecoration :: (Trail' Line v n -> Trail' Line v n) -> (Trail' Loop v n -> Trail' Loop v n) -> Decoration v n
mkDecoration = D

fromDecorating :: (InSpace v n d, Decorating d) => d -> Decoration v n
fromDecorating d = D (decLine d) (decLoop d)

