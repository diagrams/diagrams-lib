-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Util
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Some miscellaneous utilities provided by the diagrams-lib package.
--
-----------------------------------------------------------------------------

module Diagrams.Util
       ( -- * Utilities for users

         with
       , (<>)
       , applyAll
       , (#)

         -- * Internal utilities
       , Proxy(..)
       , foldB

       ) where

import Data.Monoid
import Data.Default

-- | Several functions exported by the diagrams library take a number
--   of arguments giving the user control to \"tweak\" various aspects
--   of their behavior.  Rather than give such functions a long list
--   of arguments, and to make it possible for the user to selectively
--   override only certain arguments and use default values for
--   others, such sets of arguments are collected into a record with
--   named fields (see 'PolygonOpts' in "Diagrams.TwoD.Shapes" for an
--   example).  Such record types are made instances of the 'Default'
--   class, which provides a single record structure ('def')
--   collecting the \"default\" arguments to the function.  @with@ is
--   a synonym for 'def', which provides nice-looking syntax for
--   simulating optional, named arguments in Haskell.  For example,
--
--   > polygon with {sides = 7, edgeSkip = 2}
--
--   calls the 'polygon' function with a single argument (note that
--   record update binds more tightly than function application!),
--   namely, 'with' (the record of default arguments) where the
--   @sides@ and @edgeSkip@ fields have been updated.
with :: Default d => d
with = def

-- | A convenient infix operator for 'mappend' (monoidal combination).
--   Many things in the diagrams library can be combined using @(\<\>)@,
--   with the meaning dependent on the types of things being combined.
--   For example:
--
--   * The combination of two transformations @t1 \<\> t2@ is a
--     transformation which performs first @t2@, then @t1@.
--
--   * Combining two diagrams @d1 \<\> d2@ results in a superimposed
--     diagram with @d1@ on top of @d2@ (with their local origins aligned).
--
--   * Combining two paths works in the same way as combining diagrams.
--
--   * Combining two trails results in a longer trail composed of the
--     first trail followed by the second.
--
--   * Combining two styles, @s1 \<\> s2@, results in a style with
--     combined attributes from both, biased to @s2@ when @s1@ and
--     @s2@ contain attributes of the same type.
--
--   * Combining two @'AlphaColour' Double@s results in a composited
--     color (the color that results when objects of the two colors are
--     superimposed).
--
--   In addition, 'mempty' always represents a suitably \"trivial\"
--   object which is the identity for @(\<\>)@ (that is, @mempty \<\>
--   x == x \<\> mempty == x@).  'mempty' can stand for the identity
--   transformation; the empty diagram, path, trail, or style; the
--   completely transparent color; and so on.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

infixr 5 <>

-- | @applyAll@ takes a list of functions and applies them all to a
--   value, in sequence from the last function in the list to the first.
--   For example, @applyAll [f1, f2, f3] a == f1 . f2 . f3 $ a@.
applyAll :: [a -> a] -> a -> a
applyAll = appEndo . mconcat . map Endo

infixl 8 #

-- | Postfix function application, for conveniently applying
--   attributes.  Unlike @($)@, @(#)@ has a high precedence (8), so @d
--   \# foo \# bar@ can be combined with other things using operators
--   like @(|||)@ or @(\<\>)@ without needing parentheses.
(#) :: a -> (a -> b) -> b
(#) = flip ($)

-- | A value of @Proxy a@ carries no information; it's used only to
--   fix the type @a@.
data Proxy a = Proxy

-- | Given an associative binary operation and a default value to use
--   in the case of an empty list, perform a /balanced/ fold over a
--   list.  For example,
--
--   > foldB (+) z [a,b,c,d,e,f] == ((a+b) + (c+d)) + (e+f)
--
foldB :: (a -> a -> a) -> a -> [a] -> a
foldB _ z [] = z
foldB f _ as = foldB' as
  where foldB' [x] = x
        foldB' xs  = foldB' (go xs)
        go []         = []
        go [x]        = [x]
        go (x1:x2:xs) = f x1 x2 : go xs