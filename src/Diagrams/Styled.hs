-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Styled
-- Copyright   :  (c) 2017 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- \"Styled\" things, /i.e./ things paired with an accompanying
-- 'Style'.  This is useful for attaching a style to data objects
-- which can continue to be manipulated and transformed before being
-- rendered as diagrams using the given style.  Sometimes it is more
-- convenient to attach a style earlier in the process, before the
-- final render.
--
-----------------------------------------------------------------------------

module Diagrams.Styled
    where

import Diagrams.Core.Style

-- | \"Styled\" things, /i.e./ things paired with an accompanying
--   'Style'.  This is useful for attaching a style to data objects
--   which can continue to be manipulated and transformed before being
--   rendered as diagrams using the given style.  Sometimes it is more
--   convenient to attach a style earlier in the process, before the
--   final render.
--
--   XXX examples
data Styled a =
  Styled
    { style   :: Style (V a) (N a)
    , unStyle :: a
    } deriving (Generic)

instance (Serialize a, Serialize (V a (N a))) => Serialize (Styled a)

-- | A lens giving access to the object within a 'Styled' wrapper.
_styled :: SameSpace a b => Lens (Styled a) (Styled b) a b
_styled f (Styled s a) = Styled s <$> f a

-- | Lens onto the style of something 'Styled'.
_style :: Lens' (Styled a) (Style (V a) (N a))
_style f (Styled s a) = flip Styled a <$> f s

-- | Add a default (empty) style.  Use this to inject a value of type
--   @a@ into @Styled@ before applying functions to manipulate the
--   style.  For example,
--
--   @[1,2,3] # styled # fc red # lw none@
--
--   creates a @Styled [Int]@.
styled :: a -> Styled a
styled = Styled mempty

-- | Add a given style. XXX
withStyle :: Style (V a) (N a) -> a -> Styled a
withStyle = Styled

-- | XXX
drawStyled :: (a -> QDiagram b (V a) (N a)) -> Styled a -> QDiagram b (V a) (N a)
drawStyled drawA (Styled sty a) = drawA a # applyStyle sty

deriving instance (Eq   (V a (N a)), Eq a  ) => Eq   (Styled a)
deriving instance (Ord  (V a (N a)), Ord a ) => Ord  (Styled a)

-- instance (Show (V a (N a)), Show a) => Show (Styled a) where
--   showsPrec d (Loc p a) = showParen (d > 5) $
--     showsPrec 6 a . showString " `at` " . showsPrec 6 p

-- instance (Read (V a (N a)), Read a) => Read (Styled a) where
--   readPrec = parens . prec 5 $ do
--     a <- readPrec
--     Punc "`"   <- lexP
--     Ident "at" <- lexP
--     Punc "`"   <- lexP
--     p <- readPrec
--     return (Loc p a)

type instance V (Styled a) = V a
type instance N (Styled a) = N a

instance HasStyle (Styled a) where
  applyStyle s' (Styled s a) = Styled (s' <> s) a

instance (Num (N a), Additive (V a)) => HasOrigin (Styled a) where
  moveOriginTo o (Styled s a) = Styled (moveOriginTo o s) (moveOriginTo o a)

instance (Additive (V a), Num (N a), Transformable a) => Transformable (Styled a) where
  transform t (Styled s a) = Styled (transform t s) (transform t a)

-- | The envelope of a @Styled a@ is the envelope of the @a@.
instance Enveloped a => Enveloped (Styled a) where
  getEnvelope (Styled _ a) = getEnvelope a

instance Enveloped a => Juxtaposable (Styled a) where
  juxtapose = juxtaposeDefault

-- | The trace of a @Styled a@ is the trace of the @a@.
instance (Traced a, Num (N a)) => Traced (Styled a) where
  getTrace (Styled _ a) = getTrace a

instance Alignable a => Alignable (Styled a) where
  defaultBoundary v = defaultBoundary v . unStyle

instance Qualifiable a => Qualifiable (Styled a) where
  n .>> Styled s a = Styled s (n .>> a)

type instance Codomain (Styled a) = Codomain a

-- instance (InSpace v n a, Parametric a, Codomain a ~ v)
--     => Parametric (Styled a) where
--   Loc x a `atParam` p = x .+^ (a `atParam` p)

instance DomainBounds a => DomainBounds (Styled a) where
  domainLower (Loc _ a) = domainLower a
  domainUpper (Loc _ a) = domainUpper a

instance (InSpace v n a, EndValues a, Codomain a ~ v) => EndValues (Styled a)

-- instance (InSpace v n a, Fractional n, Parametric a, Sectionable a, Codomain a ~ v)
--     => Sectionable (Styled a) where
--   splitAtParam (Loc x a) p = (Loc x a1, Loc (x .+^ (a `atParam` p)) a2)
--     where (a1,a2) = splitAtParam a p

--   reverseDomain (Loc x a) = Loc (x .+^ y) (reverseDomain a)
--     where y = a `atParam` domainUpper a

-- instance (InSpace v n a, Fractional n, HasArcLength a, Codomain a ~ v)
--     => HasArcLength (Styled a) where
--   arcLengthBounded eps (Loc _ a) = arcLengthBounded eps a
--   arcLengthToParam eps (Loc _ a) = arcLengthToParam eps a
