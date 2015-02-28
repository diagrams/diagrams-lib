{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Transform
-- Copyright   :  (c) 2011-15 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Affine transformations, parameterized by any vector space.  For
-- transformations on particular vector spaces, see /e.g./
-- "Diagrams.TwoD.Transform".
--
-----------------------------------------------------------------------------


module Diagrams.Transform
    ( -- * Transformations
      Transformation, inv, transl, apply, papply

      -- * The Transformable class
    , Transformable(..)

      -- * Some specific transformations
    , translation, translate, moveTo, place, scaling, scale

      -- * Miscellaneous transformation-related utilities
    , conjugate, underT, transformed, translated, movedTo, movedFrom

      -- * The HasOrigin class

    , HasOrigin(..), moveOriginBy

    ) where

import           Control.Lens   hiding (transform)
import           Data.Semigroup
import           Diagrams.Core

import           Linear.Vector

-- | Conjugate one transformation by another. @conjugate t1 t2@ is the
--   transformation which performs first @t1@, then @t2@, then the
--   inverse of @t1@.
conjugate :: (Additive v, Num n, Functor v)
          => Transformation v n -> Transformation v n -> Transformation v n
conjugate t1 t2 = inv t1 <> t2 <> t1

-- | Carry out some transformation \"under\" another one: @f ``under``
--   t@ first applies @t@, then @f@, then the inverse of @t@.  For
--   example, @'scaleX' 2 ``under`` 'rotation' (-1/8 \@\@ Turn)@
--   is the transformation which scales by a factor of 2 along the
--   diagonal line y = x.
--
--   Note that
--
-- @
-- (transform t2) `under` t1 == transform (conjugate t1 t2)
-- @
--
--   for all transformations @t1@ and @t2@.
underT :: (InSpace v n a, SameSpace a b, Transformable a, Transformable b)
      => (a -> b) -> Transformation v n -> a -> b
f `underT` t = transform (inv t) . f . transform t

-- | Use a 'Transformation' to make an 'Iso' between an object
--   transformed and untransformed. This is useful for carrying out
--   functions 'under' another transform:
--
-- @
-- under (transformed t) f               == transform (inv t) . f . transform t
-- under (transformed t1) (transform t2) == transform (conjugate t1 t2)
-- transformed t ## a                    == transform t a
-- a ^. transformed t                    == transform (inv t) a
-- @
transformed :: (InSpace v n a, SameSpace a b, Transformable a, Transformable b)
            => Transformation v n -> Iso a b a b
transformed t = iso (transform $ inv t) (transform t)

-- | Use a 'Point' to make an 'Iso' between an object
--   moved to and from that point:
--
-- @
-- under (movedTo p) f == moveTo (-p) . f . moveTo p
-- over (movedTo p) f  == moveTo p . f . moveTo (-p)
-- movedTo p           == from (movedFrom p)
-- movedTo p ## a      == moveTo p a
-- a ^. movedTo p      == moveOriginTo p a
-- @
movedTo :: (InSpace v n a, SameSpace a b, HasOrigin a, HasOrigin b)
        => Point v n -> Iso a b a b
movedTo p = iso (moveTo (negated p)) (moveTo p)

-- | Use a 'Transformation' to make an 'Iso' between an object
--   transformed and untransformed. We have
--
-- @
-- under (movedFrom p) f == moveTo p . f . moveTo (-p)
-- movedFrom p           == from (movedTo p)
-- movedFrom p ## a      == moveOriginTo p a
-- a ^. movedFrom p      == moveTo p a
-- over (movedFrom p) f  == moveTo (-p) . f . moveTo p
-- @
movedFrom :: (InSpace v n a, SameSpace a b, HasOrigin a, HasOrigin b)
          => Point v n -> Iso a b a b
movedFrom p = iso (moveOriginTo (negated p)) (moveOriginTo p)

-- | Use a vector to make an 'Iso' between an object translated and
--   untranslated.
--
-- @
-- under (translated v) f == translate (-v) . f . translate v
-- translated v ## a      == translate v a
-- a ^. translated v      == translate (-v) a
-- over (translated v) f  == translate v . f . translate (-v)
-- @
translated :: (InSpace v n a, SameSpace a b, Transformable a, Transformable b)
           => v n -> Iso a b a b
translated = transformed . translation
