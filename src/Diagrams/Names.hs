{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Names
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Names can be given to subdiagrams, and subdiagrams can later be
-- queried by name.  This module exports types for representing names
-- and subdiagrams, and various functions for working with them.
--
-----------------------------------------------------------------------------

module Diagrams.Names
    ( -- * Names

      AName, Name, IsName(..), (.>)
    , Qualifiable(..)

      -- * Subdiagrams

    , Subdiagram, mkSubdiagram, subPoint, getSub, rawSub, location

      -- * Subdiagram maps

    , SubMap, fromNames, rememberAs, lookupSub

      -- * Naming things

    , named, nameSub, namePoint, localize

      -- * Querying by name

    , names
    , lookupName
    , withName, withNameAll, withNames

    ) where

import           Data.Semigroup

import           Diagrams.Core       (OrderedField, Point)
import           Diagrams.Core.Names
import           Diagrams.Core.Types

import           Linear.Metric

-- | Attach an atomic name to a diagram.
named :: (IsName nm, Metric v, OrderedField n, Semigroup m)
      => nm -> QDiagram b v n m -> QDiagram b v n m
named = nameSub mkSubdiagram

-- | Attach an atomic name to a certain point (which may be computed
--   from the given diagram), treated as a subdiagram with no content
--   and a point envelope.
namePoint :: (IsName nm , Metric v, OrderedField n, Semigroup m, Monoid m)
          => (QDiagram b v n m -> Point v n) -> nm -> QDiagram b v n m -> QDiagram b v n m
namePoint p = nameSub (subPoint . p)

