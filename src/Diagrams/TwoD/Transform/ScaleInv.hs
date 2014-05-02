{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Transform.ScaleInv
-- Copyright   :  (c) 2012-2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Wrapper for creating scale-invariant objects in two dimensions.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Transform.ScaleInv
    ( ScaleInv(..)
    , scaleInvObj, scaleInvDir, scaleInvLoc
    , scaleInv, scaleInvPrim )
    where

import           Control.Lens            (makeLenses, view)
import           Data.AdditiveGroup
import           Data.AffineSpace        ((.-.))
import           Data.Semigroup
import           Data.Typeable

import           Diagrams.Core
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector

-- | The @ScaleInv@ wrapper creates two-dimensional /scale-invariant/
--   objects.  Intuitively, a scale-invariant object is affected by
--   transformations like translations and rotations, but not by scales.
--
--   However, this is problematic when it comes to /non-uniform/
--   scales (/e.g./ @scaleX 2 . scaleY 3@) since they can introduce a
--   perceived rotational component.  The prototypical example is an
--   arrowhead on the end of a path, which should be scale-invariant.
--   However, applying a non-uniform scale to the path but not the
--   arrowhead would leave the arrowhead pointing in the wrong
--   direction.
--
--   Moreover, for objects whose local origin is not at the local
--   origin of the parent diagram, any scale can result in a
--   translational component as well.
--
--   The solution is to also store a point (indicating the location,
--   /i.e./ the local origin) and a unit vector (indicating the
--   /direction/) along with a scale-invariant object.  A
--   transformation to be applied is decomposed into rotational and
--   translational components as follows:
--
--   * The transformation is applied to the direction vector, and the
--   difference in angle between the original direction vector and its
--   image under the transformation determines the rotational
--   component.  The rotation is applied with respect to the stored
--   location, rather than the global origin.
--
--   * The vector from the location to the image of the location under
--   the transformation determines the translational component.

data ScaleInv t =
  ScaleInv
  { _scaleInvObj :: t
  , _scaleInvDir :: R2
  , _scaleInvLoc :: P2
  }
  deriving (Show, Typeable)

makeLenses ''ScaleInv

-- | Create a scale-invariant object pointing in the given direction,
--   located at the origin.
scaleInv :: t -> R2 -> ScaleInv t
scaleInv t d = ScaleInv t d origin

type instance V (ScaleInv t) = R2

instance (V t ~ R2, HasOrigin t) => HasOrigin (ScaleInv t) where
  moveOriginTo p (ScaleInv t v l) = ScaleInv (moveOriginTo p t) v (moveOriginTo p l)

instance (V t ~ R2, Transformable t) => Transformable (ScaleInv t) where
  transform tr (ScaleInv t v l) = ScaleInv (trans . rot $ t) (rot v) l'
    where
      angle = direction (transform tr v) ^-^ direction v
      rot :: (Transformable t, V t ~ R2) => t -> t
      rot = rotateAbout l angle
      l'  = transform tr l
      trans = translate (l' .-. l)

{- Proof that the above satisfies the monoid action laws.

   1. transform mempty (ScaleInv t v l)
      = ScaleInv (trans . rot $ t) (rot v) l'
        { l'    = transform mempty l = l }
        { trans = translate (l' .-. l)
                = translate (l .-. l)
                = translate zeroV
                = id
        }
        { rot   = rotateAbout l angle
                = rotateAbout l (direction (transform mempty v) - direction v)
                = rotateAbout l (direction v - direction v)
                = rotateAbout l 0
                = id
        }
      = ScaleInv t v l

  2. transform t1 (transform t2 (ScaleInv t v l))
     = let angle = direction (transform t2 v) - direction v
           rot   = rotateAbout l angle
           l'    = transform t2 l
           trans = translate (l' .-. l)
       in
           transform t1 (ScaleInv (trans . rot $ t) (rot v) l')

     = let angle  = direction (transform t2 v) - direction v
           rot    = rotateAbout l angle
           l'     = transform t2 l
           trans  = translate (l' .-. l)
           angle2 = direction (transform t1 (rot v)) - direction (rot v)
           rot2   = rotateAbout l' angle2
           l'2    = transform t1 l'
           trans2 = translate (l'2 .-. l')
       in
           ScaleInv (trans2 . rot2 . trans . rot $ t) (rot2 . rot $ v) l'2

       { l'2 = transform t1 l'
             = transform t1 (transform t2 l)
             = transform (t1 <> t2) l
       }
       { trans2 = translate (l'2 .-. l')
                = translate (transform (t1 <> t2) l .-. transform t2 l)
                = translate (transform t1 l .-. l)
       }
       { rot v  = rotateAbout l angle v
                = rotate angle `under` translation (origin .-. l) $ v
                = rotate angle v
       }
       { angle2 = direction (transform t1 (rot v)) - direction (rot v)
                = direction (transform t1 (rotate angle v)) - direction (rotate angle v)
                = direction (transform t1 (rotate angle v)) - direction v - angle
       }
       { rot2   = rotateAbout l' angle2
                = ???
       }

-}

-- | Create a diagram from a single scale-invariant primitive.  The
--   vector argument specifies the direction in which the primitive is
--   \"pointing\" (for the purpose of keeping it rotated correctly
--   under non-uniform scaling).  The primitive is assumed to be
--   \"located\" at the origin (for the purpose of translating it
--   correctly under scaling).
--
--   Note that the resulting diagram will have an /empty/ envelope,
--   trace, and query.  The reason is that the envelope, trace, and
--   query cannot be cached---applying a transformation would cause
--   the cached envelope, etc. to get \"out of sync\" with the
--   scale-invariant object.  The intention, at any rate, is that
--   scale-invariant things will be used only as \"decorations\" (/e.g./
--   arrowheads) which should not affect the envelope, trace, and
--   query.
scaleInvPrim :: (Transformable t, Typeable t, V t ~ R2, Monoid m)
             => t -> R2 -> QDiagram R2 m
scaleInvPrim t d = mkQD (Prim $ scaleInv t d) mempty mempty mempty mempty
