-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Query
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A query is a function that maps points in a vector space to values
-- in some monoid. Queries naturally form a monoid, with two queries
-- being combined pointwise.
--
-----------------------------------------------------------------------------

module Diagrams.Query
    ( Query(..), query, sample, value, resetValue, clearValue

    ) where

import           Diagrams.Core
