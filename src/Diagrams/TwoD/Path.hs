{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , DeriveDataTypeable
           , GeneralizedNewtypeDeriving
           , TypeFamilies
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Paths in two dimensions are special since we may stroke them to
-- create a 2D diagram, and (eventually) perform operations such as
-- intersection and union.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Path
       ( -- * Constructing path-based diagrams

         stroke, stroke', strokeT, strokeT'

       , StrokeOpts(..)

         -- * Inside/outside testing

       , isInsideWinding, isInsideEvenOdd

         -- * Clipping

       , Clip(..), clipBy
       ) where

import Graphics.Rendering.Diagrams

import Diagrams.Segment
import Diagrams.Path
import Diagrams.TwoD.Types
import Diagrams.Solve

import Data.AdditiveGroup
import Data.VectorSpace
import Data.AffineSpace

import Data.Semigroup hiding ((<>))
import Control.Applicative (liftA2)
import qualified Data.Foldable as F
import Data.Default

import Data.Typeable

------------------------------------------------------------
--  Constructing path-based diagrams  ----------------------
------------------------------------------------------------

-- | Convert a path into a diagram.  The resulting diagram has the
--   names 0, 1, ... assigned to each of the path's vertices.
--
--   See also 'stroke'', which takes an extra options record allowing
--   its behavior to be customized.
--
--   Note that a bug in GHC 7.0.1 causes a context stack overflow when
--   inferring the type of @stroke@.  The solution is to give a type
--   signature to expressions involving @stroke@, or (recommended)
--   upgrade GHC (the bug is fixed in 7.0.2 onwards).
stroke :: Renderable (Path R2) b
       => Path R2 -> Diagram b R2
stroke = stroke' (def :: StrokeOpts ())

instance Renderable (Path R2) b => PathLike (AnnDiagram b R2 Any) where
  pathLike st cl segs = stroke $ pathLike st cl segs

-- | A variant of 'stroke' that takes an extra record of options to
--   customize its behavior.  In particular:
--
--     * Names can be assigned to the path's vertices
--
--   'StrokeOpts' is an instance of 'Default', so @stroke' 'with' {
--   ... }@ syntax may be used.
stroke' :: (Renderable (Path R2) b, Atomic a) => StrokeOpts a -> Path R2 -> Diagram b R2
stroke' opts p
  = mkAD (Prim p)
         (getBounds p)
         (fromNames . concat $
           zipWith zip (vertexNames opts) (pathVertices p))
         (Query $ Any . flip isInsideWinding p)

-- | A record of options that control how a path is stroked.
--   @StrokeOpts@ is an instance of 'Default', so a @StrokeOpts@
--   records can be created using @'with' { ... }@ notation.
data StrokeOpts a
  = StrokeOpts
    { vertexNames :: [[a]]  -- ^ Atomic names that should be assigned
                            --   to the vertices of the path so that
                            --   they can be referenced later.  If
                            --   there are not enough names, the extra
                            --   vertices are not assigned names; if
                            --   there are too many, the extra names
                            --   are ignored.  Note that this is a
                            --   /list of lists/ of names, since paths
                            --   can consist of multiple trails.  The
                            --   first list of names are assigned to
                            --   the vertices of the first trail, the
                            --   second list to the second trail, and
                            --   so on.
                            --
                            --   The default value is the empty list.
    }

instance Default (StrokeOpts a) where
  def = StrokeOpts
        { vertexNames = []
        }

-- | A composition of 'stroke' and 'pathFromTrail' for conveniently
--   converting a trail directly into a diagram.
--
--   Note that a bug in GHC 7.0.1 causes a context stack overflow when
--   inferring the type of 'stroke' and hence of @strokeT@ as well.
--   The solution is to give a type signature to expressions involving
--   @strokeT@, or (recommended) upgrade GHC (the bug is fixed in 7.0.2
--   onwards).
strokeT :: (Renderable (Path R2) b)
        => Trail R2 -> Diagram b R2
strokeT = stroke . pathFromTrail

-- | A composition of 'stroke'' and 'pathFromTrail' for conveniently
--   converting a trail directly into a diagram.
strokeT' :: (Renderable (Path R2) b, Atomic a)
         => StrokeOpts a -> Trail R2 -> Diagram b R2
strokeT' opts = stroke' opts . pathFromTrail

------------------------------------------------------------
--  Inside/outside testing
------------------------------------------------------------

cross :: R2 -> R2 -> Double
cross (x,y) (x',y') = x * y' - y * x'

-- XXX link to more info on this

-- | Test whether the given point is inside the given (closed) path,
--   by testing whether the point's /winding number/ is nonzero. Note
--   that @False@ is /always/ returned for /open/ paths, regardless of
--   the winding number.
isInsideWinding :: P2 -> Path R2 -> Bool
isInsideWinding p = (/= 0) . crossings p

-- | Test whether the given point is inside the given (closed) path,
--   by testing whether a ray extending from the point in the positive
--   x direction crosses the path an even (outside) or odd (inside)
--   number of times.  Note that @False@ is /always/ returned for
--   /open/ paths, regardless of the number of crossings.
isInsideEvenOdd :: P2 -> Path R2 -> Bool
isInsideEvenOdd p = odd . crossings p

data FixedSegment v = FLinear (Point v) (Point v)
                    | FCubic (Point v) (Point v) (Point v) (Point v)
  deriving Show

mkFixedSeg :: AdditiveGroup v => Point v -> Segment v -> FixedSegment v
mkFixedSeg p (Linear v)       = FLinear p (p .+^ v)
mkFixedSeg p (Cubic c1 c2 x2) = FCubic p (p .+^ c1) (p .+^ c2) (p .+^ x2)

fAtParam :: VectorSpace v => FixedSegment v -> Scalar v -> Point v
fAtParam (FLinear p1 p2) t = alerp p1 p2 t
fAtParam (FCubic x1 c1 c2 x2) t = p3
  where p11 = alerp x1 c1 t
        p12 = alerp c1 c2 t
        p13 = alerp c2 x2 t

        p21 = alerp p11 p12 t
        p22 = alerp p12 p13 t

        p3  = alerp p21 p22 t

-- | Compute the sum of /signed/ crossings of a path as we travel in the
--   positive x direction from a given point.
crossings :: P2 -> Path R2 -> Int
crossings p = F.sum . map (trailCrossings p) . pathTrails

-- | Compute the sum of signed crossings of a trail starting from the
--   given point in the positive x direction.
trailCrossings :: P2 -> (P2, Trail R2) -> Int

  -- open trails have no inside or outside, so don't contribute crossings
trailCrossings _ (_, t) | not (isClosed t) = 0

trailCrossings p@(P (x,y)) (start, tr)
  = sum . map test
  $ zipWith mkFixedSeg (trailVertices start tr)
                       (trailSegments tr ++ [Linear . negateV . trailOffset $ tr])
  where
    test (FLinear a@(P (_,ay)) b@(P (_,by)))
      | ay <= y && by > y && isLeft a b > 0 =  1
      | by <= y && ay > y && isLeft a b < 0 = -1
      | otherwise                           =  0

    test c@(FCubic (P x1@(_,x1y)) (P c1@(_,c1y)) (P c2@(_,c2y)) (P x2@(_,x2y))) =
        sum . map testT $ ts
      where ts = filter (liftA2 (&&) (>=0) (<=1))
               $ cubForm (-  x1y + 3*c1y - 3*c2y + x2y)
                         ( 3*x1y - 6*c1y + 3*c2y)
                         (-3*x1y + 3*c1y)
                         (x1y - y)
            testT t = let (P (px,_)) = c `fAtParam` t
                      in  if px > x then signFromDerivAt t else 0
            signFromDerivAt t =
              let (dx,dy) = (3*t*t) *^ ((-1)*^x1 ^+^ 3*^c1 ^-^ 3*^c2 ^+^ x2)
                        ^+^ (2*t)   *^ (3*^x1 ^-^ 6*^c1 ^+^ 3*^c2)
                        ^+^            ((-3)*^x1 ^+^ 3*^c1)
                  ang = atan2 dy dx
              in  case () of _ | (0 < ang && ang < pi && t < 1)  -> 1
                               | (-pi < ang && ang < 0 && t > 0) -> -1
                               | otherwise                       -> 0

    isLeft a b = cross (b .-. a) (p .-. a)

------------------------------------------------------------
--  Clipping  ----------------------------------------------
------------------------------------------------------------

-- | @Clip@ tracks the accumulated clipping paths applied to a
--   diagram.  Note that the semigroup structure on @Clip@ is list
--   concatenation, so applying multiple clipping paths is sensible.
--   The clipping region is the intersection of all the applied
--   clipping paths.
newtype Clip = Clip { getClip :: [Path R2] }
  deriving (Typeable, Semigroup)
instance AttributeClass Clip

type instance V Clip = R2

instance Transformable Clip where
  transform t (Clip ps) = Clip (transform t ps)

-- | Clip a diagram by the given path:
--
--   * Only the parts of the diagram which lie in the interior of the
--     path will be drawn.
--
--   * The bounding function of the diagram is unaffected.
clipBy :: (HasStyle a, V a ~ R2) => Path R2 -> a -> a
clipBy = applyTAttr . Clip . (:[])

-- XXX Should include a 'clipTo' function which clips a diagram AND
-- restricts its bounding function.  It will have to take a *pointwise
-- minimum* of the diagram's current bounding function and the path's
-- bounding function.  Not sure of the best way to do this at the moment.