{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , UndecidableInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Combinators
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Higher-level tools for combining diagrams.
--
-----------------------------------------------------------------------------

module Diagrams.Combinators
       ( -- * Unary operations

         withBounds
       , phantom, strut

       , pad

         -- * Binary operations
       , beside, besideBounds
       , append

         -- * n-ary operations
       , appends
       , position, decorateTrail
       , cat, cat', CatOpts(..), CatMethod(..)

       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Segment (Segment(..))
import Diagrams.Path
import Diagrams.Align
import Diagrams.Util

import Data.AdditiveGroup
import Data.AffineSpace ((.-.))
import Data.VectorSpace

import Data.Monoid
import Data.List

import Data.Default

------------------------------------------------------------
-- Working with bounds
------------------------------------------------------------

-- XXX this isn't quite right!  Need to think about this a bit more...
-- | Use the bounding region from some boundable object as the
--   bounding region for a diagram, in place of the diagram's default
--   bounding region.
withBounds :: (Backend b (V a), Boundable a, Monoid m)
           => a -> AnnDiagram b (V a) m -> AnnDiagram b (V a) m
withBounds b d = d `atop` phantom b

-- | @phantom x@ produces a \"phantom\" diagram, which has the same
--   bounding region as @x@ but produces no output.
phantom :: (Backend b (V a), Boundable a, Monoid m) => a -> AnnDiagram b (V a) m
phantom a = mkAD nullPrim (getBounds a) mempty mempty

-- | @pad s@ \"pads\" a diagram, expanding its bounding region by a
--   factor of @s@.  Note that the bounding region will expand with
--   respect to the local origin, so if the origin is not centered the
--   padding may appear \"uneven\".  If this is not desired, the
--   origin can be centered (using, e.g., 'centerXY' for 2D diagrams)
--   before applying @pad@.
pad :: ( Backend b v
       , InnerSpace v, OrderedField (Scalar v)
       , Monoid m )
    => Scalar v -> AnnDiagram b v m -> AnnDiagram b v m
pad s d = withBounds (d # scale s) d

-- | @strut v@ is a diagram which produces no output, but for the
--   purposes of alignment and bounding regions acts like a
--   1-dimensional segment oriented along the vector @v@.  Useful for
--   manually creating separation between two diagrams.
strut :: ( Backend b v, InnerSpace v
         , OrderedField (Scalar v)
         , Monoid m
         )
      => v -> AnnDiagram b v m
strut v = phantom $ getBounds (Linear v)

------------------------------------------------------------
-- Combining two objects
------------------------------------------------------------

-- | Place two bounded, monoidal objects (i.e. diagrams or paths) next
--   to each other along the given vector.  In particular, place the
--   first object so that the vector points from its local origin to
--   the local origin of the second object, at a distance so that
--   their bounding regions are just tangent.  The local origin of the
--   new, combined object is at the point of tangency, along the line
--   between the old local origins.
beside :: (HasOrigin a, Boundable a, Monoid a) => V a -> a -> a -> a
beside v d1 d2
  = align v d1 <> align (negateV v) d2
-- XXX add picture to above documentation?

-- Note that sending the origin to the point of tangency like this
-- means that (beside v) is not associative.  We can make it
-- associative if we specify that the origin of the new, composed
-- diagram is the same as the local origin of the first diagram (or,
-- dually, of the second).  But then mempty is only a right identity,
-- not a left identity.  (To be sure, with the current implementation
-- mempty is no identity at all!)  We could make (beside v) a monoidal
-- operation (associative, with mempty as identity) if we always
-- center the origin along v after combining.  That sounds nice from a
-- theoretical point of view but not from a usability point of view...

-- | @besideBounds b v x@ positions @x@ so it is beside the bounding
--   region @b@ in the direction of @v@.  The origin of the new
--   diagram is the origin of the bounding region.
besideBounds :: (HasOrigin a, Boundable a) => Bounds (V a) -> V a -> a -> a
besideBounds b v a
  = moveOriginBy (origin .-. boundary v b) (align (negateV v) a)

-- | Like 'beside', but the origin of the final combined object is the
--   origin of the first object.  See also 'appends'.
append :: (HasOrigin a, Boundable a, Monoid a) => V a -> a -> a -> a
append v d1 d2 = appends d1 [(v,d2)]

------------------------------------------------------------
-- Combining multiple objects
------------------------------------------------------------

-- | @appends x ys@ appends each of the objects in @ys@ to the object
--   @x@ in the corresponding direction.  Note that each object in
--   @ys@ is positioned beside @x@ /without/ reference to the other
--   objects in @ys@, so this is not the same as iterating @append@.
appends :: (HasOrigin a, Boundable a, Monoid a) => a -> [(V a,a)] -> a
appends d1 apps = d1 <> mconcat (map (uncurry (besideBounds b)) apps)
  where b = getBounds d1

-- | Position things absolutely: combine a list of objects
-- (e.g. diagrams or paths) by assigning them absolute positions in
-- the vector space of the combined object.
position :: (HasOrigin a, Qualifiable a, Monoid a) => [(Point (V a), a)] -> a
position = mconcat . zipWith (|>) [1::Integer ..] . map (uncurry moveTo)

-- | Combine a list of diagrams (or paths) by using them to
--   \"decorate\" a trail, placing the local origin of one object at
--   each successive vertex of the trail.  If the trail and list of
--   objects have different lengths, the extra tail of the longer one
--   is ignored.
decorateTrail :: (HasOrigin a, Qualifiable a, Monoid a) => Trail (V a) -> [a] -> a
decorateTrail t = position . zip (trailVertices origin t)

-- | Methods for concatenating diagrams.
data CatMethod = Cat     -- ^ Normal catenation: simply put diagrams
                         --   next to one another (possibly with a
                         --   certain distance in between each). The
                         --   distance between successive diagram
                         --   /boundaries/ will be consistent; the
                         --   distance between /origins/ may vary if
                         --   the diagrams are of different sizes.
               | Distrib -- ^ Distribution: place the local origins of
                         --   diagrams at regular intervals.  With
                         --   this method, the distance between
                         --   successive /origins/ will be consistent
                         --   but the distance between boundaries may
                         --   not be.  Indeed, depending on the amount
                         --   of separation, diagrams may overlap.

-- | Options for 'cat''.
data CatOpts v = CatOpts { catMethod       :: CatMethod
                             -- ^ Which 'CatMethod' should be used:
                             --   normal catenation (default), or
                             --   distribution?
                         , sep             :: Scalar v
                             -- ^ How much separation should be used
                             --   between successive diagrams
                             --   (default: 0)?  When @catMethod =
                             --   Cat@, this is the distance between
                             --   /boundaries/; when @catMethod =
                             --   Distrib@, this is the distance
                             --   between /origins/.
                         , catOptsvProxy__ :: Proxy v
                             -- ^ This field exists solely to aid type inference;
                             --   please ignore it.
                         }

-- The reason the proxy field is necessary is that without it,
-- altering the sep field could theoretically change the type of a
-- CatOpts record.  This causes problems when writing an expression
-- like @with { sep = 10 }@, because knowing the type of the whole
-- expression does not tell us anything about the type of @with@, and
-- therefore the @Num (Scalar v)@ constraint cannot be satisfied.
-- Adding the Proxy field constrains the type of @with@ in @with {sep
-- = 10}@ to be the same as the type of the whole expression.

instance Num (Scalar v) => Default (CatOpts v) where
  def = CatOpts { catMethod       = Cat
                , sep             = 0
                , catOptsvProxy__ = Proxy
                }

-- | @cat v@ positions a list of objects so that their local origins
--   lie along a line in the direction of @v@.  Successive objects
--   will have their bounding regions just touching.  The local origin
--   of the result will be the same as the local origin of the first
--   object.
--
--   See also 'cat'', which takes an extra options record allowing
--   certain aspects of the operation to be tweaked.
cat :: (HasOrigin a, Boundable a, Qualifiable a, Monoid a) => V a -> [a] -> a
cat v = cat' v def

-- | Like 'cat', but taking an extra 'CatOpts' arguments allowing the
--   user to specify
--
--   * The spacing method: catenation (uniform spacing between
--     boundaries) or distribution (uniform spacing between local
--     origins).  The default is catenation.
--
--   * The amount of separation between successive diagram
--     boundaries/origins (depending on the spacing method).  The
--     default is 0.
--
--   'CatOpts' is an instance of 'Default', so 'with' may be used for
--   the second argument, as in @cat' (1,2) with {sep = 2}@.
--
--   Note that @cat' v with {catMethod = Distrib} === mconcat@
--   (distributing with a separation of 0 is the same as
--   superimposing).
cat' :: (HasOrigin a, Boundable a, Qualifiable a, Monoid a)
     => V a -> CatOpts (V a) -> [a] -> a
cat' _ (CatOpts { catMethod = Cat }) []     = mempty
cat' _ (CatOpts { catMethod = Cat }) [d]    = (1::Integer) |> d
cat' v (CatOpts { catMethod = Cat, sep = s }) (d:ds) =
  foldl' (\d1 d2 ->
           d1 <> (moveOriginBy (origin .-. boundary v d1)
                  . moveOriginBy (withLength s (negateV v))
                  . align (negateV v)
                  $ d2)
         )
         ((1::Integer) |> d)
         (zipWith (|>) [2::Integer ..] ds)

cat' v (CatOpts { catMethod = Distrib, sep = s }) ds =
  decorateTrail (fromOffsets (repeat (withLength s v))) ds
  -- infinite trail, no problem for Haskell =D
