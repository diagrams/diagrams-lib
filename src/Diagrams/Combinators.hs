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

         withEnvelope, withTrace
       , phantom, strut
       , pad
       , extrudeEnvelope, intrudeEnvelope

         -- * Binary operations
       , beneath
       , beside

         -- * n-ary operations
       , appends
       , position, decorateTrail, decoratePath
       , cat, cat', CatOpts(..), CatMethod(..)

       ) where

import Diagrams.Core

import Diagrams.Segment (Segment(..))
import Diagrams.Path
import Diagrams.Util

import Data.AdditiveGroup
import Data.VectorSpace

import Data.Semigroup

import Data.Default

------------------------------------------------------------
-- Working with envelopes
------------------------------------------------------------

-- | Use the envelope from some object as the envelope for a
--   diagram, in place of the diagram's default envelope.
withEnvelope :: (HasLinearMap (V a), Enveloped a, Monoid' m)
           => a -> QDiagram b (V a) m -> QDiagram b (V a) m
withEnvelope = setEnvelope . getEnvelope

-- | Use the trace from some object as the trace for a diagram, in
--   place of the diagram's default trace.
withTrace :: (HasLinearMap (V a), Traced a, OrderedField (Scalar (V a)), InnerSpace (V a), Monoid' m)
          => a -> QDiagram b (V a) m -> QDiagram b (V a) m
withTrace = setTrace . getTrace

-- | @phantom x@ produces a \"phantom\" diagram, which has the same
--   envelope and trace as @x@ but produces no output.
phantom :: (Backend b (V a), Enveloped a, Traced a, Monoid' m) => a -> QDiagram b (V a) m
phantom a = mkQD nullPrim (getEnvelope a) (getTrace a) mempty mempty

-- | @pad s@ \"pads\" a diagram, expanding its envelope by a factor of
--   @s@ (factors between 0 and 1 can be used to shrink the envelope).
--   Note that the envelope will expand with respect to the local
--   origin, so if the origin is not centered the padding may appear
--   \"uneven\".  If this is not desired, the origin can be centered
--   (using, e.g., 'centerXY' for 2D diagrams) before applying @pad@.
pad :: ( Backend b v
       , InnerSpace v, OrderedField (Scalar v)
       , Monoid' m )
    => Scalar v -> QDiagram b v m -> QDiagram b v m
pad s d = withEnvelope (d # scale s) d

-- | @strut v@ is a diagram which produces no output, but with respect
--   to alignment and envelope acts like a 1-dimensional segment
--   oriented along the vector @v@, with local origin at its
--   center. (Note, however, that it has an empty trace; for 2D struts
--   with a nonempty trace see 'strutR2', 'strutX', and 'strutY' from
--   "Diagrams.TwoD.Combinators".) Useful for manually creating
--   separation between two diagrams.
strut :: ( Backend b v, InnerSpace v
         , OrderedField (Scalar v)
         , Monoid' m
         )
      => v -> QDiagram b v m
strut v = mkQD nullPrim env mempty mempty mempty
  where env = translate ((-0.5) *^ v) . getEnvelope $ Linear v
  -- note we can't use 'phantom' here because it tries to construct a
  -- trace as well, and segments do not have a trace in general (only
  -- in 2D; see Diagrams.TwoD.Segment).  This is a good reason to have
  -- a special 'strut' combinator (before the introduction of traces
  -- it was mostly just for convenience).
  --
  -- also note that we can't remove the call to getEnvelope, since
  -- translating a segment has no effect.

-- | @extrudeEnvelope v d@ asymmetrically \"extrudes\" the envelope of
--   a diagram in the given direction.  All parts of the envelope
--   within 90 degrees of this direction are modified, offset outwards
--   by the magnitude of the vector.
--
--   This works by offsetting the envelope distance proportionally to
--   the cosine of the difference in angle, and leaving it unchanged
--   when this factor is negative.
extrudeEnvelope
  :: ( Ord (Scalar v), Num (Scalar v), AdditiveGroup (Scalar v)
     , Floating (Scalar v), HasLinearMap v, InnerSpace v, Monoid' m )
  => v -> QDiagram b v m -> QDiagram b v m
extrudeEnvelope = deformEnvelope 1

-- | @intrudeEnvelope v d@ asymmetrically \"intrudes\" the envelope of
--   a diagram away from the given direction.  All parts of the envelope
--   within 90 degrees of this direction are modified, offset inwards
--   by the magnitude of the vector.
--
--   Note that this could create strange inverted envelopes, where
--   @ diameter v d < 0 @.
intrudeEnvelope
  :: ( Ord (Scalar v), Num (Scalar v), AdditiveGroup (Scalar v)
     , Floating (Scalar v), HasLinearMap v, InnerSpace v, Monoid' m )
  => v -> QDiagram b v m -> QDiagram b v m
intrudeEnvelope = deformEnvelope (-1)

-- Utility for extrudeEnvelope / intrudeEnvelope
deformEnvelope
  :: ( Ord (Scalar v), Num (Scalar v), AdditiveGroup (Scalar v)
     , Floating (Scalar v), HasLinearMap v, InnerSpace v, Monoid' m )
  => (Scalar v) -> v -> QDiagram b v m -> QDiagram b v m
deformEnvelope s v d = setEnvelope (inEnvelope deform $ getEnvelope d) d
  where
    deform = Option . fmap deform' . getOption
    deform' env v'
        | dot > 0 = Max $ getMax (env v') + dot * s
        | otherwise = env v'
      where
        dot = v' <.> v

------------------------------------------------------------
-- Combining two objects
------------------------------------------------------------

-- | @beneath@ is just a convenient synonym for @'flip' 'atop'@; that is,
--   @d1 \`beneath\` d2@ is the diagram with @d2@ superimposed on top of
--   @d1@.
beneath :: (HasLinearMap v, OrderedField (Scalar v), InnerSpace v, Monoid' m)
     => QDiagram b v m -> QDiagram b v m -> QDiagram b v m
beneath = flip atop

infixl 6 `beneath`

-- | Place two monoidal objects (/i.e./ diagrams, paths,
--   animations...) next to each other along the given vector.  In
--   particular, place the second object so that the vector points
--   from the local origin of the first object to the local origin of
--   the second object, at a distance so that their envelopes are just
--   tangent.  The local origin of the new, combined object is the
--   local origin of the first object.
--
--   Note that @beside v@ is associative, so objects under @beside v@
--   form a semigroup for any given vector @v@.  However, they do
--   /not/ form a monoid, since there is no identity element. 'mempty'
--   is a right identity (@beside v d1 mempty === d1@) but not a left
--   identity (@beside v mempty d1 === d1 # align (negateV v)@).
--
--   In older versions of diagrams, @beside@ put the local origin of
--   the result at the point of tangency between the two inputs.  That
--   semantics can easily be recovered by performing an alignment on
--   the first input before combining.  That is, if @beside'@ denotes
--   the old semantics,
--
--   > beside' v x1 x2 = beside v (x1 # align v) x2
--
--   To get something like @beside v x1 x2@ whose local origin is
--   identified with that of @x2@ instead of @x1@, use @beside
--   (negateV v) x2 x1@.
beside :: (Juxtaposable a, Semigroup a) => V a -> a -> a -> a
beside v d1 d2 = d1 <> juxtapose v d1 d2

-- XXX add picture to above documentation?

------------------------------------------------------------
-- Combining multiple objects
------------------------------------------------------------

-- | @appends x ys@ appends each of the objects in @ys@ to the object
--   @x@ in the corresponding direction.  Note that each object in
--   @ys@ is positioned beside @x@ /without/ reference to the other
--   objects in @ys@, so this is not the same as iterating 'beside'.
appends :: (Juxtaposable a, Monoid' a) => a -> [(V a,a)] -> a
appends d1 apps = d1 <> mconcat (map (\(v,d) -> juxtapose v d1 d) apps)

-- | Position things absolutely: combine a list of objects
--   (e.g. diagrams or paths) by assigning them absolute positions in
--   the vector space of the combined object.
position :: (HasOrigin a, Monoid' a) => [(Point (V a), a)] -> a
position = mconcat . map (uncurry moveTo)

-- | Combine a list of diagrams (or paths) by using them to
--   \"decorate\" a trail, placing the local origin of one object at
--   each successive vertex of the trail.  The first vertex of the
--   trail is placed at the origin.  If the trail and list of objects
--   have different lengths, the extra tail of the longer one is
--   ignored.
decorateTrail :: (HasOrigin a, Monoid' a) => Trail (V a) -> [a] -> a
decorateTrail t = position . zip (trailVertices origin t)

-- | Combine a list of diagrams (or paths) by using them to
--   \"decorate\" a path, placing the local origin of one object at
--   each successive vertex of the path.  If the path and list of objects
--   have different lengths, the extra tail of the longer one is
--   ignored.
decoratePath :: (HasOrigin a, Monoid' a) => Path (V a) -> [a] -> a
decoratePath p = position . zip (concat $ pathVertices p)

-- | Methods for concatenating diagrams.
data CatMethod = Cat     -- ^ Normal catenation: simply put diagrams
                         --   next to one another (possibly with a
                         --   certain distance in between each). The
                         --   distance between successive diagram
                         --   /envelopes/ will be consistent; the
                         --   distance between /origins/ may vary if
                         --   the diagrams are of different sizes.
               | Distrib -- ^ Distribution: place the local origins of
                         --   diagrams at regular intervals.  With
                         --   this method, the distance between
                         --   successive /origins/ will be consistent
                         --   but the distance between envelopes may
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
                             --   /envelopes/; when @catMethod =
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
--   will have their envelopes just touching.  The local origin
--   of the result will be the same as the local origin of the first
--   object.
--
--   See also 'cat'', which takes an extra options record allowing
--   certain aspects of the operation to be tweaked.
cat :: ( Juxtaposable a, Monoid' a, HasOrigin a
       , InnerSpace (V a), Floating (Scalar (V a))
       )
       => V a -> [a] -> a
cat v = cat' v def

-- | Like 'cat', but taking an extra 'CatOpts' arguments allowing the
--   user to specify
--
--   * The spacing method: catenation (uniform spacing between
--     envelopes) or distribution (uniform spacing between local
--     origins).  The default is catenation.
--
--   * The amount of separation between successive diagram
--     envelopes/origins (depending on the spacing method).  The
--     default is 0.
--
--   'CatOpts' is an instance of 'Default', so 'with' may be used for
--   the second argument, as in @cat' (1,2) with {sep = 2}@.
--
--   Note that @cat' v with {catMethod = Distrib} === mconcat@
--   (distributing with a separation of 0 is the same as
--   superimposing).
cat' :: ( Juxtaposable a, Monoid' a, HasOrigin a
        , InnerSpace (V a), Floating (Scalar (V a))
        )
     => V a -> CatOpts (V a) -> [a] -> a
cat' v (CatOpts { catMethod = Cat, sep = s }) = foldB comb mempty
  where comb d1 d2 = d1 <> (juxtapose v d1 d2 # moveOriginBy vs)
        vs = s *^ normalized (negateV v)

cat' v (CatOpts { catMethod = Distrib, sep = s }) =
  decorateTrail . fromOffsets . repeat $ s *^ normalized v
  -- infinite trail, no problem for Haskell =D
