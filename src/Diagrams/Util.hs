-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Util
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Miscellaneous utilities.
--
-----------------------------------------------------------------------------

module Diagrams.Util where

import Data.Monoid
import Data.Default

-- | XXX comment me
with :: Default d => d
with = def

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- | Placeholder for things which still need to be implemented.
writeMe :: String -> a
writeMe s = error $ "The " ++ s ++ " function is not yet implemented.  Maybe you would like to implement it?"