{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric         #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Located
-- Copyright   :  (c) 2013-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- \"Located\" things, /i.e./ things with a concrete location:
-- intuitively, @Located a ~ (a, Point)@.  Wrapping a translationally
-- invariant thing (/e.g./ a 'Segment' or 'Trail') in @Located@ pins
-- it down to a particular location and makes it no longer
-- translationally invariant.
--
-----------------------------------------------------------------------------

module Diagrams.Located
    ( Located (..)
    , at, viewLoc, mapLoc, located, _loc
    )
    where

import           Control.Lens            (Lens, Lens')
#if __GLASGOW_HASKELL__ < 710
import           Data.Functor            ((<$>))
#endif
import           Text.Read

import           Linear.Affine
import           Linear.Vector

import           Diagrams.Align
import           Diagrams.Core
import           Diagrams.Core.Transform
import           Diagrams.Parametric

import           GHC.Generics (Generic)
import           Data.Serialize (Serialize)

-- | \"Located\" things, /i.e./ things with a concrete location:
--   intuitively, @Located a ~ (Point, a)@.  Wrapping a translationally
--   invariant thing (/e.g./ a 'Segment' or 'Trail') in 'Located' pins
--   it down to a particular location and makes it no longer
--   translationally invariant.
--
--   @Located@ is intentionally abstract.  To construct @Located@
--   values, use 'at'.  To destruct, use 'viewLoc', 'unLoc', or 'loc'.
--   To map, use 'mapLoc'.
--
--   Much of the utility of having a concrete type for the @Located@
--   concept lies in the type class instances we can give it.  The
--   'HasOrigin', 'Transformable', 'Enveloped', 'Traced', and
--   'TrailLike' instances are particularly useful; see the documented
--   instances below for more information.
data Located a =
  Loc { loc   :: Point (V a) (N a)  -- ^ Project out the
                                --   location of a @Located@
                                --   value.
      , unLoc :: a              -- ^ Project the value
                                --   of type @a@ out of
                                --   a @Located a@,
                                --   discarding the
                                --   location.
      } deriving (Generic)

instance (Serialize a, Serialize (V a (N a))) => Serialize (Located a)

infix 5 `at`

-- | Construct a @Located a@ from a value of type @a@ and a location.
--   @at@ is intended to be used infix, like @x \`at\` origin@.
at :: a -> Point (V a) (N a) -> Located a
at a p = Loc p a

-- | Deconstruct a @Located a@ into a location and a value of type
--   @a@.  @viewLoc@ can be especially useful in conjunction with the
--   @ViewPatterns@ extension.
viewLoc :: Located a -> (Point (V a) (N a), a)
viewLoc (Loc p a) = (p,a)

-- | 'Located' is not a @Functor@, since changing the type could
--   change the type of the associated vector space, in which case the
--   associated location would no longer have the right type. 'mapLoc'
--   has an extra constraint specifying that the vector space must
--   stay the same.
--
--   (Technically, one can say that for every vector space @v@,
--   @Located@ is a little-f (endo)functor on the category of types
--   with associated vector space @v@; but that is not covered by the
--   standard @Functor@ class.)
mapLoc :: SameSpace a b => (a -> b) -> Located a -> Located b
mapLoc f (Loc p a) = Loc p (f a)

-- | A lens giving access to the object within a 'Located' wrapper.
located :: SameSpace a b => Lens (Located a) (Located b) a b
located f (Loc p a) = Loc p <$> f a

-- | Lens onto the location of something 'Located'.
_loc :: Lens' (Located a) (Point (V a) (N a))
_loc f (Loc p a) = flip Loc a <$> f p

deriving instance (Eq   (V a (N a)), Eq a  ) => Eq   (Located a)
deriving instance (Ord  (V a (N a)), Ord a ) => Ord  (Located a)

instance (Show (V a (N a)), Show a) => Show (Located a) where
  showsPrec d (Loc p a) = showParen (d > 5) $
    showsPrec 6 a . showString " `at` " . showsPrec 6 p

instance (Read (V a (N a)), Read a) => Read (Located a) where
  readPrec = parens . prec 5 $ do
    a <- readPrec
    Punc "`"   <- lexP
    Ident "at" <- lexP
    Punc "`"   <- lexP
    p <- readPrec
    return (Loc p a)

type instance V (Located a) = V a
type instance N (Located a) = N a

-- | @Located a@ is an instance of @HasOrigin@ whether @a@ is or not.
--   In particular, translating a @Located a@ simply translates the
--   associated point (and does /not/ affect the value of type @a@).
instance (Num (N a), Additive (V a)) => HasOrigin (Located a) where
  moveOriginTo o (Loc p a) = Loc (moveOriginTo o p) a

-- | Applying a transformation @t@ to a @Located a@ results in the
--   transformation being applied to the location, and the /linear/
--   /portion/ of @t@ being applied to the value of type @a@ (/i.e./
--   it is not translated).
instance (Additive (V a), Num (N a), Transformable a) => Transformable (Located a) where
  transform t@(Transformation t1 t2 _) (Loc p a)
    = Loc (transform t p) (transform (Transformation t1 t2 zero) a)

-- | The envelope of a @Located a@ is the envelope of the @a@,
--   translated to the location.
instance Enveloped a => Enveloped (Located a) where
  getEnvelope (Loc p a) = moveTo p (getEnvelope a)

instance Enveloped a => Juxtaposable (Located a) where
  juxtapose = juxtaposeDefault

-- | The trace of a @Located a@ is the trace of the @a@,
--   translated to the location.
instance (Traced a, Num (N a)) => Traced (Located a) where
  getTrace (Loc p a) = moveTo p (getTrace a)

instance Alignable a => Alignable (Located a) where
  defaultBoundary v = defaultBoundary v . unLoc

instance Qualifiable a => Qualifiable (Located a) where
  n .>> Loc p a = Loc p (n .>> a)

type instance Codomain (Located a) = Point (Codomain a)

instance (InSpace v n a, Parametric a, Codomain a ~ v)
    => Parametric (Located a) where
  Loc x a `atParam` p = x .+^ (a `atParam` p)

instance DomainBounds a => DomainBounds (Located a) where
  domainLower (Loc _ a) = domainLower a
  domainUpper (Loc _ a) = domainUpper a

instance (InSpace v n a, EndValues a, Codomain a ~ v) => EndValues (Located a)

instance (InSpace v n a, Fractional n, Parametric a, Sectionable a, Codomain a ~ v)
    => Sectionable (Located a) where
  splitAtParam (Loc x a) p = (Loc x a1, Loc (x .+^ (a `atParam` p)) a2)
    where (a1,a2) = splitAtParam a p

  section (Loc x a) p1 p2 = Loc (x .+^ (a `atParam` p1)) (section a p1 p2)

  reverseDomain (Loc x a) = Loc (x .+^ y) (reverseDomain a)
    where y = a `atParam` domainUpper a

instance (InSpace v n a, Fractional n, HasArcLength a, Codomain a ~ v)
    => HasArcLength (Located a) where
  arcLengthBounded eps (Loc _ a) = arcLengthBounded eps a
  arcLengthToParam eps (Loc _ a) = arcLengthToParam eps a
