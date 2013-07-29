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

      -- * Naming things

    , named, nameSub, namePoint, localize

      -- * Querying by name

    , names
    , withName, withNameAll, withNames

    ) where

import           Diagrams.Core.Names
import           Diagrams.Core.Types
