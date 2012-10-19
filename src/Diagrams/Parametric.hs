{-# LANGUAGE DefaultSignatures
           , FlexibleContexts
           , TypeFamilies
           , UndecidableInstances
  #-}
module Diagrams.Parametric
  ( 
  -- * Parametric functions
  	Codomain, Parametric(..), ArcLength(..), ArcLengthToParam(..)

  , DomainBounds(..), EndValues(..), Sectionable(..)

  -- * Adjusting
  , Adjustable(..), AdjustOpts(..), AdjustMethod(..), AdjustSide(..)
  ) where

import Diagrams.Core
import Diagrams.Util

import Data.Default
import Data.VectorSpace

-- | Codomain of parametric classes.  This is usually either (V p), for relative
--   vector results, or (Point (V p)), for functions with absolute coordinates.
type family Codomain p :: *

-- | Type class for parameteric functions.
class Parametric p where
-- | 'atParam' yields a parametrized view of relative paths as continuous
--   functions. It is designed to be used infix, like @path ``atParam`` 0.5@.
  atParam :: p -> Scalar (V p) -> Codomain p

-- | Type class for parametric functions with a bounded domain.
--
--   Note that this domain indicates the main \"interesting\" portion of the
--   function.  It must be defined within this range, but for some instances may
--   still have sensible values outside.
class DomainBounds p where
  -- | 'domainLower' defaults to being constantly 0 (for vectorspaces with
  --   numeric scalars).
  domainLower :: p -> Scalar (V p)

  default domainLower :: Num (Scalar (V p)) => p -> Scalar (V p)
  domainLower = const 0

  -- | 'domainUpper' defaults to (0, 1), as the unit domain is common. 
  domainUpper :: p -> Scalar (V p)

  default domainUpper :: Num (Scalar (V p)) => p -> Scalar (V p)
  domainUpper = const 1

-- | Type class for querying the value of the function at the ends of its domain.
class EndValues p where
  -- | 'startPoint' is the vector to / point at the start of the path.
  --
  --   @ offset path = path ``atParam`` fst (domainBounds path) @
  --
  --   This is the default implementation, but some representations will
  --   have a more efficient and precise implementation.
  atStart :: p -> Codomain p

  default atStart :: (Parametric p, DomainBounds p) => p -> Codomain p
  atStart path = path `atParam` domainLower path

  -- | 'endPoint' is the vector to / point at the end of the path.
  --
  --   @ offset path = path ``atParam`` snd (domainBounds path) @
  --
  --   This is the default implementation, but some representations will
  --   have a more efficient and precise implementation.
  atEnd :: p -> Codomain p

  default atEnd :: (Parametric p, DomainBounds p) => p -> Codomain p
  atEnd path = path `atParam` domainUpper path

domainBounds :: DomainBounds p => p -> (Scalar (V p), Scalar (V p))
domainBounds path = (domainLower path, domainUpper path)

-- | Type class for domain manipulation of parametric functions.
--
--   Minimal definition: Either 'splitAtParam' or 'section'.
class DomainBounds p => Sectionable p where
  -- | 'splitAtParam' splits a path @p@ into two new paths @(l,r)@
  --   at the parameter @t@ where @l@ corresponds to the portion of
  --   @f@ for parameter values from @0@ to @t@ and @r@ for @s@ from @t@ to @1@.
  --   The following should hold for splitting:
  --
  -- > paramSplit f t u =
  -- >   | u < t     = atParam f u == atParam l (u / t)
  -- >   | otherwise = atParam f u == atParam f t ^+^ atParam l ((u - t) / (snd (domainBounds f) - t))
  -- >   where (l,r) = splitAtParam f t
  --
  --   That is to say, the parameterization scales linearly with splitting.
  --
  --   'splitAtParam' can also be used with parameters outside the range
  --   of the domain.  For example, using the parameter @2@ gives two result
  --   paths where the first is the original path extended to the
  --   parameter 2, and the second result path travels /backwards/
  --   from the end of the first to the end of the original function.
  splitAtParam :: p -> Scalar (V p) -> (p, p)
  splitAtParam path t
    = ( section path (domainLower path) t
      , section path t (domainUpper path))

  -- > \path l u t -> let s = section path l u
  -- >   in domainLower path == domainLower s
  -- >   && domainUpper path == domainUpper s
  -- >   && (path `atParam` lerp l u t) == (s `atParam` t)
  section :: p -> Scalar (V p) -> Scalar (V p) -> p
  default section :: Fractional (Scalar (V p)) => p -> Scalar (V p) -> Scalar (V p) -> p
  section path t1 t2 = snd (splitAtParam (fst (splitAtParam path t2)) (t1/t2))

  -- Flip about the domain bounds.  This has the following default definition
  --
  -- > reverse path = section path (domainUpper path) (domainLower path)
  reverseDomain :: p -> p
  reverseDomain path = section path (domainUpper path) (domainLower path)


-- | Type class for computing the arc length of a parametric path.
class Parametric p => ArcLength p where
  -- Approximates the arc length up to the given accuracy (plus or minus).
  arcLength :: p -> Scalar (V p) -> Scalar (V p)

-- | Type class for computing the parameter for a given arc length.
class ArcLength p => ArcLengthToParam p where
  -- | @'arcLengthToParam' s l m@ converts the absolute arc length @l@,
  --   measured from the segment starting point, to a parameter on the
  --   segment @s@, with accuracy of at least plus or minus @m@.  Works
  --   for /any/ arc length, and may return any parameter value (not
  --   just parameters between 0 and 1).
  arcLengthToParam :: p -> Scalar (V p) -> Scalar (V p) -> Scalar (V p)

--------------------------------------------------
--  Adjusting length
--------------------------------------------------

class Adjustable a where
  -- | Adjust the length of a segment / trail / path.  The second parameter is
  --   an option record which controls how the adjustment should be performed;
  --   see 'AdjustOpts'.
  adjust :: a -> AdjustOpts (V a) -> a

-- | What method should be used for adjusting a segment, trail, or
--   path?
data AdjustMethod v = ByParam (Scalar v)     -- ^ Extend by the given parameter value
                                             --   (use a negative parameter to shrink)
                    | ByAbsolute (Scalar v)  -- ^ Extend by the given arc length
                                             --   (use a negative length to shrink)
                    | ToAbsolute (Scalar v)  -- ^ Extend or shrink to the given
                                             --   arc length

-- | Which side of a segment, trail, or path should be adjusted?
data AdjustSide = Start  -- ^ Adjust only the beginning
                | End    -- ^ Adjust only the end
                | Both   -- ^ Adjust both sides equally
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | How should a segment, trail, or path be adjusted?
data AdjustOpts v = ALO { adjMethod :: AdjustMethod v
                        , adjSide   :: AdjustSide
                        , adjEps    :: Scalar v
                        , adjOptsvProxy__ :: Proxy v
                        }

instance Fractional (Scalar v) => Default (AdjustMethod v) where
  def = ByParam 0.2

instance Default AdjustSide where
  def = Both

instance Fractional (Scalar v) => Default (AdjustOpts v) where
  def = ALO def def (1/10^(10 :: Integer)) Proxy
