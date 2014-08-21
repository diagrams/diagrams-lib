-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Transform
-- Copyright   :  (c) 2011-13 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Affine transformations, parameterized by any vector space.  For
-- transformations on particular vector spaces, see /e.g./
-- "Diagrams.TwoD.Transform".
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module Diagrams.Transform
    ( -- * Transformations
      Transformation, inv, transl, apply, papply

      -- * The Transformable class
    , Transformable(..)

      -- * Some specific transformations
    , translation, translate, moveTo, place, scaling, scale

      -- * Miscellaneous transformation-related utilities
    , conjugate, under

      -- * The HasOrigin class

    , HasOrigin(..), moveOriginBy

    ) where

import Data.Semigroup
import Diagrams.Core

-- | Conjugate one transformation by another. @conjugate t1 t2@ is the
--   transformation which performs first @t1@, then @t2@, then the
--   inverse of @t1@.
conjugate :: (HasLinearMap v, Num n, Functor v)
          => Transformation v n -> Transformation v n -> Transformation v n
conjugate t1 t2  = inv t1 <> t2 <> t1

-- | Carry out some transformation \"under\" another one: @f ``under``
--   t@ first applies @t@, then @f@, then the inverse of @t@.  For
--   example, @'scaleX' 2 ``under`` 'rotation' (-1/8 \@\@ Turn)@
--   is the transformation which scales by a factor of 2 along the
--   diagonal line y = x.
--
--   Note that
--
--   @
--   (transform t2) `under` t1 == transform (conjugate t1 t2)
--   @
--
--   for all transformations @t1@ and @t2@.
under :: (Transformable a, Transformable b, Vn a ~ Vn b, Vn a ~ v n, Num n, Functor v)
      => (a -> b) -> Transformation v n -> a -> b
f `under` t = transform (inv t) . f . transform t
