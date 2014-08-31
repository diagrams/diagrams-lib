{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
    , scaleInv, scaleInvPrim)
    where

import           Control.Lens            (makeLenses, view, (^.))
import           Data.Semigroup
import           Data.Typeable

import           Diagrams.Angle
import           Diagrams.Core
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types

import           Linear.Affine
import           Linear.Vector

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
  , _scaleInvDir :: Vn t
  , _scaleInvLoc :: Point (V t) (N t)
  }
  deriving Typeable

deriving instance (Show t, Show (Vn t)) => Show (ScaleInv t)

makeLenses ''ScaleInv

-- | Create a scale-invariant object pointing in the given direction,
--   located at the origin.
scaleInv :: (Vn t ~ v n, Additive v, Num n) => t -> v n -> ScaleInv t
scaleInv t d = ScaleInv t d origin

type instance V (ScaleInv t) = V t
type instance N (ScaleInv t) = N t

instance (Vn t ~ v n, Additive v, Num n, HasOrigin t) => HasOrigin (ScaleInv t) where
  moveOriginTo p (ScaleInv t v l) = ScaleInv (moveOriginTo p t) v (moveOriginTo p l)

instance (Vn t ~ V2 n, RealFloat n, Transformable t) => Transformable (ScaleInv t) where
  transform tr (ScaleInv t v l) = ScaleInv (trans . rot $ t) (rot v) l'
    where
      -- angle :: Angle n
      angle = transform tr v ^. _theta

      rot :: (Vn k ~ Vn t, Transformable k) => k -> k
      rot = rotateAround l angle

      -- l' :: Point V2 n
      l'  = transform tr l

      -- trans :: (Vn k ~ Vn t, Transformable k) => k -> k
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
        { rot   = rotateAround l angle
                = rotateAround l (direction (transform mempty v) - direction v)
                = rotateAround l (direction v - direction v)
                = rotateAround l 0
                = id
        }
      = ScaleInv t v l

  2. transform t1 (transform t2 (ScaleInv t v l))
     = let angle = direction (transform t2 v) - direction v
           rot   = rotateAround l angle
           l'    = transform t2 l
           trans = translate (l' .-. l)
       in
           transform t1 (ScaleInv (trans . rot $ t) (rot v) l')

     = let angle  = direction (transform t2 v) - direction v
           rot    = rotateAround l angle
           l'     = transform t2 l
           trans  = translate (l' .-. l)
           angle2 = direction (transform t1 (rot v)) - direction (rot v)
           rot2   = rotateAround l' angle2
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
       { rot v  = rotateAround l angle v
                = rotate angle `under` translation (origin .-. l) $ v
                = rotate angle v
       }
       { angle2 = direction (transform t1 (rot v)) - direction (rot v)
                = direction (transform t1 (rotate angle v)) - direction (rotate angle v)
                = direction (transform t1 (rotate angle v)) - direction v - angle
       }
       { rot2   = rotateAround l' angle2
                = ???
       }

-}

instance (Vn t ~ V2 n, RealFloat n, Renderable t b) => Renderable (ScaleInv t) b where
  render b = render b . view scaleInvObj

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
scaleInvPrim :: (Vn t ~ V2 n, RealFloat n, Transformable t, Typeable t, Renderable t b, Monoid m)
             => t -> V2 n -> QDiagram b (V t) (N t) m
scaleInvPrim t d = mkQD (Prim $ scaleInv t d) mempty mempty mempty mempty
