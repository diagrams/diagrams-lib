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

-- XXX add export list

-- XXX should this be split into internal vs external utilities?

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

infixl 1 #

-- | Postfix function application, for conveniently applying
--   attributes.  @(#)@ has a slightly higher precedence than @($)@, so
--   @someFunction $ d # foo # bar@ parses as @someFunction (d # foo # bar)@.
(#) :: a -> (a -> b) -> b
(#) = flip ($)

-- | A value of @Proxy a@ carries no information; it's used only to
--   fix the type @a@.
data Proxy a = Proxy