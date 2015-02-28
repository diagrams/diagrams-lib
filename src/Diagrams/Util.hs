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
       , applyAll
       , (#)
       , (##)

       , iterateN

       , tau

         -- * Internal utilities
       , foldB

       ) where

import           Data.Default.Class
import           Data.Monoid
import           Control.Lens hiding (( # ))

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
--   @
--   polygon with {sides = 7, edgeSkip = 2}
--   @
--
--   calls the 'polygon' function with a single argument (note that
--   record update binds more tightly than function application!),
--   namely, 'with' (the record of default arguments) where the
--   @sides@ and @edgeSkip@ fields have been updated.
with :: Default d => d
with = def

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

-- | A replacement for lenses' @#@ operator. @(##) = 'review'@.
(##) :: AReview t b -> b -> t
(##) = review
{-# INLINE (##) #-}
infixr 8 ##


-- | @iterateN n f x@ returns the list of the first @n@ iterates of
--   @f@ starting at @x@, that is, the list @[x, f x, f (f x), ...]@
--   of length @n@. (Note that the last element of the list will be
--   @f@ applied to @x@ @(n-1)@ times.)
iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f = take n . iterate f

-- | The circle constant, the ratio of a circle's circumference to its
--   /radius/.  Note that @pi = tau/2@.
--
--   For more information and a well-reasoned argument why we should
--   all be using tau instead of pi, see /The Tau Manifesto/,
--   <http://tauday.com/>.
--
--   To hear what it sounds like (and to easily memorize the first 30
--   digits or so), try <http://youtu.be/3174T-3-59Q>.
tau :: Floating a => a
tau = 2*pi

-- | Given an associative binary operation and a default value to use
--   in the case of an empty list, perform a /balanced/ fold over a
--   list.  For example,
--
--   @
--   foldB (+) z [a,b,c,d,e,f] == ((a+b) + (c+d)) + (e+f)
--   @
--
foldB :: (a -> a -> a) -> a -> [a] -> a
foldB _ z [] = z
foldB f _ as = foldB' as
  where foldB' [x] = x
        foldB' xs  = foldB' (go xs)
        go []         = []
        go [x]        = [x]
        go (x1:x2:xs) = f x1 x2 : go xs
