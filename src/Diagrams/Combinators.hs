{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
       , pad, frame
       , extrudeEnvelope, intrudeEnvelope

         -- * Binary operations
       , atop
       , beneath
       , beside

         -- * n-ary operations
       , appends
       , position, decorateTrail, decorateLocatedTrail, decoratePath
       , cat, cat'
       , CatOpts(_catMethod, _sep), catMethod, sep
       , CatMethod(..)

       ) where

import           Data.Typeable

import           Control.Lens          (Lens', generateSignatures, lensField,
                                        lensRules, makeLensesWith, (%~), (&),
                                        (.~), (^.), _Wrapping)
import           Data.AdditiveGroup
import           Data.AffineSpace      ((.+^))
import           Data.Default.Class
import           Data.Monoid.Deletable (toDeletable)
import           Data.Monoid.MList     (inj)
#if __GLASGOW_HASKELL__ < 707
import           Data.Proxy
#endif
import           Data.Semigroup
import qualified Data.Tree.DUAL        as D
import           Data.VectorSpace

import           Diagrams.Core
import           Diagrams.Core.Types   (QDiagram (QD))
import           Diagrams.Located
import           Diagrams.Path
import           Diagrams.Segment      (straight)
import           Diagrams.Trail        (Trail, trailVertices)
import           Diagrams.Util

------------------------------------------------------------
-- Working with envelopes
------------------------------------------------------------

-- | Use the envelope from some object as the envelope for a
--   diagram, in place of the diagram's default envelope.
--
--   <<diagrams/src_Diagrams_Combinators_withEnvelopeEx.svg#diagram=withEnvelopeEx&width=300>>
--
--   > sqNewEnv =
--   >     circle 1 # fc green
--   >     |||
--   >     (    c # dashing [0.1,0.1] 0 # lc white
--   >       <> square 2 # withEnvelope (c :: D R2) # fc blue
--   >     )
--   > c = circle 0.8
--   > withEnvelopeEx = sqNewEnv # centerXY # pad 1.5
withEnvelope :: (HasLinearMap (V a), Enveloped a, Monoid' m)
           => a -> QDiagram (V a) m -> QDiagram (V a) m
withEnvelope = setEnvelope . getEnvelope

-- | Use the trace from some object as the trace for a diagram, in
--   place of the diagram's default trace.
withTrace :: (HasLinearMap (V a), Traced a, OrderedField (Scalar (V a)), InnerSpace (V a), Monoid' m)
          => a -> QDiagram (V a) m -> QDiagram (V a) m
withTrace = setTrace . getTrace

-- | @phantom x@ produces a \"phantom\" diagram, which has the same
--   envelope and trace as @x@ but produces no output.
phantom :: (Typeable (V a), Enveloped a, Traced a, Monoid' m) => a -> QDiagram (V a) m
phantom a = QD $ D.leafU ((inj . toDeletable . getEnvelope $ a) <> (inj . toDeletable . getTrace $ a))

-- | @pad s@ \"pads\" a diagram, expanding its envelope by a factor of
--   @s@ (factors between 0 and 1 can be used to shrink the envelope).
--   Note that the envelope will expand with respect to the local
--   origin, so if the origin is not centered the padding may appear
--   \"uneven\".  If this is not desired, the origin can be centered
--   (using, e.g., 'centerXY' for 2D diagrams) before applying @pad@.
pad :: ( HasLinearMap v, InnerSpace v, OrderedField (Scalar v)
       , Monoid' m )
    => Scalar v -> QDiagram v m -> QDiagram v m
pad s d = withEnvelope (d # scale s) d

-- | @frame s@ increases the envelope of a diagram by and absolute amount @s@,
--   s is in the local units of the diagram. This function is similar to @pad@,
--   only it takes an absolute quantity and pre-centering should not be
--   necessary.
frame :: (HasLinearMap v, InnerSpace v, OrderedField (Scalar v), Monoid' m)
        => Scalar v -> QDiagram v m -> QDiagram v m
frame s d = setEnvelope (onEnvelope t (d^.envelope)) d
  where
    t f = \x -> f x + s

-- | @strut v@ is a diagram which produces no output, but with respect
--   to alignment and envelope acts like a 1-dimensional segment
--   oriented along the vector @v@, with local origin at its
--   center. (Note, however, that it has an empty trace; for 2D struts
--   with a nonempty trace see 'strutR2', 'strutX', and 'strutY' from
--   "Diagrams.TwoD.Combinators".) Useful for manually creating
--   separation between two diagrams.
--
--   <<diagrams/src_Diagrams_Combinators_strutEx.svg#diagram=strutEx&width=300>>
--
--   > strutEx = (circle 1 ||| strut unitX ||| circle 1) # centerXY # pad 1.1
strut :: ( HasLinearMap v, Typeable v
         , InnerSpace v
         , OrderedField (Scalar v)
         , Monoid' m
         )
      => v -> QDiagram v m
strut v = QD $ D.leafU (inj . toDeletable $ env)
  where env = translate ((-0.5) *^ v) . getEnvelope $ straight v
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
  => v -> QDiagram v m -> QDiagram v m
extrudeEnvelope = deformEnvelope 0.5

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
  => v -> QDiagram v m -> QDiagram v m
intrudeEnvelope = deformEnvelope (-0.5)

-- Utility for extrudeEnvelope / intrudeEnvelope
deformEnvelope
  :: ( Ord (Scalar v), Num (Scalar v), AdditiveGroup (Scalar v)
     , Floating (Scalar v), HasLinearMap v, InnerSpace v, Monoid' m )
  => (Scalar v) -> v -> QDiagram v m -> QDiagram v m
deformEnvelope s v d = setEnvelope (getEnvelope d & _Wrapping Envelope %~ deformE) d
  where
    deformE = Option . fmap deformE' . getOption
    deformE' env v'
        | dot > 0 = Max $ getMax (env v') + (dot * s) / magnitude v'
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
     => QDiagram v m -> QDiagram v m -> QDiagram v m
beneath = flip atop

infixl 6 `beneath`

-- | Place two monoidal objects (/i.e./ diagrams, paths,
--   animations...) next to each other along the given vector.  In
--   particular, place the second object so that the vector points
--   from the local origin of the first object to the local origin of
--   the second object, at a distance so that their envelopes are just
--   tangent.  The local origin of the new, combined object is the
--   local origin of the first object (unless the first object is the
--   identity element, in which case the second object is returned
--   unchanged).
--
--   <<diagrams/src_Diagrams_Combinators_besideEx.svg#diagram=besideEx&height=200>>
--
--   > besideEx = beside (r2 (20,30))
--   >                   (circle 1 # fc orange)
--   >                   (circle 1.5 # fc purple)
--   >            # showOrigin
--   >            # centerXY # pad 1.1
--
--   Note that @beside v@ is associative, so objects under @beside v@
--   form a semigroup for any given vector @v@.  In fact, they also
--   form a monoid: 'mempty' is clearly a right identity (@beside v d1
--   mempty === d1@), and there should also be a special case to make
--   it a left identity, as described above.
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

------------------------------------------------------------
-- Combining multiple objects
------------------------------------------------------------

-- | @appends x ys@ appends each of the objects in @ys@ to the object
--   @x@ in the corresponding direction.  Note that each object in
--   @ys@ is positioned beside @x@ /without/ reference to the other
--   objects in @ys@, so this is not the same as iterating 'beside'.
--
--   <<diagrams/src_Diagrams_Combinators_appendsEx.svg#diagram=appendsEx&width=200>>
--
--   > appendsEx = appends c (zip (iterateN 6 (rotateBy (1/6)) unitX) (repeat c))
--   >             # centerXY # pad 1.1
--   >   where c = circle 1
appends :: (Juxtaposable a, Monoid' a) => a -> [(V a,a)] -> a
appends d1 apps = d1 <> mconcat (map (\(v,d) -> juxtapose v d1 d) apps)

-- | Position things absolutely: combine a list of objects
--   (e.g. diagrams or paths) by assigning them absolute positions in
--   the vector space of the combined object.
--
--   <<diagrams/src_Diagrams_Combinators_positionEx.svg#diagram=positionEx&height=300>>
--
--   > positionEx = position (zip (map mkPoint [-3, -2.8 .. 3]) (repeat dot))
--   >   where dot       = circle 0.2 # fc black
--   >         mkPoint x = p2 (x,x^2)
position :: (HasOrigin a, Monoid' a) => [(Point (V a), a)] -> a
position = mconcat . map (uncurry moveTo)

-- | Combine a list of diagrams (or paths) by using them to
--   \"decorate\" a trail, placing the local origin of one object at
--   each successive vertex of the trail.  The first vertex of the
--   trail is placed at the origin.  If the trail and list of objects
--   have different lengths, the extra tail of the longer one is
--   ignored.
decorateTrail :: (InnerSpace (V a), OrderedField (Scalar (V a)), HasOrigin a, Monoid' a)
              => Trail (V a) -> [a] -> a
decorateTrail = decorateLocatedTrail . (`at` origin)

-- | Combine a list of diagrams (or paths) by using them to
--   \"decorate\" a concretely located trail, placing the local origin
--   of one object at each successive vertex of the trail. If the
--   trail and list of objects have different lengths, the extra tail
--   of the longer one is ignored.
decorateLocatedTrail :: (InnerSpace (V a), OrderedField (Scalar (V a)), HasOrigin a, Monoid' a)
              => Located (Trail (V a)) -> [a] -> a
decorateLocatedTrail t = position . zip (trailVertices t)

-- | Combine a list of diagrams (or paths) by using them to
--   \"decorate\" a path, placing the local origin of one object at
--   each successive vertex of the path.  If the path and list of objects
--   have different lengths, the extra tail of the longer one is
--   ignored.
decoratePath :: (InnerSpace (V a), OrderedField (Scalar (V a)), HasOrigin a, Monoid' a)
             => Path (V a) -> [a] -> a
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
data CatOpts v = CatOpts { _catMethod       :: CatMethod
                         , _sep             :: Scalar v
                         , _catOptsvProxy__ :: Proxy v
                         }

-- The reason the proxy field is necessary is that without it,
-- altering the sep field could theoretically change the type of a
-- CatOpts record.  This causes problems when using record update, as
-- in @with { _sep = 10 }@, because knowing the type of the whole
-- expression does not tell us anything about the type of @with@, and
-- therefore the @Num (Scalar v)@ constraint cannot be satisfied.
-- Adding the Proxy field constrains the type of @with@ in @with {_sep
-- = 10}@ to be the same as the type of the whole expression.  Note
-- this is not a problem when using the 'sep' lens, as its type is
-- more restricted.

makeLensesWith
  ( lensRules
    -- don't make a lens for the proxy field
    & lensField .~ (\label ->
        case label of
          "_catOptsvProxy__" -> Nothing
          _ -> Just (drop 1 label)
        )
    & generateSignatures .~ False
  )
  ''CatOpts

-- | Which 'CatMethod' should be used:
--   normal catenation (default), or distribution?
catMethod :: forall v. Lens' (CatOpts v) CatMethod

-- | How much separation should be used between successive diagrams
--   (default: 0)?  When @catMethod = Cat@, this is the distance between
--   /envelopes/; when @catMethod = Distrib@, this is the distance
--   between /origins/.
sep :: forall v. Lens' (CatOpts v) (Scalar v)

instance Num (Scalar v) => Default (CatOpts v) where
  def = CatOpts { _catMethod       = Cat
                , _sep             = 0
                , _catOptsvProxy__ = Proxy
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
       , InnerSpace (V a), OrderedField (Scalar (V a))
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
--   the second argument, as in @cat' (1,2) (with & sep .~ 2)@.
--
--   Note that @cat' v (with & catMethod .~ Distrib) === mconcat@
--   (distributing with a separation of 0 is the same as
--   superimposing).
cat' :: ( Juxtaposable a, Monoid' a, HasOrigin a
        , InnerSpace (V a), OrderedField (Scalar (V a))
        )
     => V a -> CatOpts (V a) -> [a] -> a
cat' v (CatOpts { _catMethod = Cat, _sep = s }) = foldB comb mempty
  where comb d1 d2 = d1 <> (juxtapose v d1 d2 # moveOriginBy vs)
        vs = s *^ normalized (negateV v)

cat' v (CatOpts { _catMethod = Distrib, _sep = s }) =
  position . zip (iterate (.+^ (s *^ normalized v)) origin)
