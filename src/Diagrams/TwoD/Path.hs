{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Path
-- Copyright   :  (c) 2011-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Paths in two dimensions are special since we may stroke them to
-- create a 2D diagram, and (eventually) perform operations such as
-- intersection and union.  They also have a trace, whereas paths in
-- higher dimensions do not.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Path
  ( -- * Constructing path-based diagrams

    stroke, stroke'
  , strokePath, strokeP, strokePath', strokeP'
  , strokeTrail, strokeT, strokeTrail', strokeT'
  , strokeLine, strokeLoop
  , strokeLocTrail, strokeLocT, strokeLocLine, strokeLocLoop

    -- ** Stroke options

  , FillRule(..)
  , getFillRule, fillRule, _fillRule
  , StrokeOpts(..), vertexNames, queryFillRule

    -- ** Inside/outside testing

  , Crossings (..)
  , isInsideWinding
  , isInsideEvenOdd

    -- * Clipping

  , Clip(..), _Clip, _clip
  , clipBy, clipTo, clipped

    -- * Intersections

  , intersectPoints, intersectPoints'
  , intersectPointsP, intersectPointsP'
  , intersectPointsT, intersectPointsT'
  ) where

import           Control.Applicative       (liftA2)
import           Control.Lens              hiding (at, transform)
import qualified Data.Foldable             as F
import           Data.Semigroup
import           Data.Typeable

import           Data.Default.Class

import           Diagrams.Angle
import           Diagrams.Combinators      (withEnvelope, withTrace)
import           Diagrams.Core
import           Diagrams.Core.Trace
import           Diagrams.Located          (Located, mapLoc, unLoc)
import           Diagrams.Parametric
import           Diagrams.Path
import           Diagrams.Query
import           Diagrams.Segment
import           Diagrams.Solve.Polynomial
import           Diagrams.Trail
import           Diagrams.TrailLike
import           Diagrams.TwoD.Segment
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector
import           Diagrams.Util             (tau)

import           Linear.Affine
import           Linear.Vector

------------------------------------------------------------
--  Trail and path traces  ---------------------------------
------------------------------------------------------------

-- Only 2D trails and paths have a trace.

-- XXX can the efficiency of this be improved?  See the comment in
-- Diagrams.Path on the Enveloped instance for Trail.
instance RealFloat n => Traced (Trail V2 n) where
  getTrace = withLine $
      foldr
        (\seg bds -> moveOriginBy (negated . atEnd $ seg) bds <> getTrace seg)
        mempty
    . lineSegments

instance RealFloat n => Traced (Path V2 n) where
  getTrace = F.foldMap getTrace . op Path

------------------------------------------------------------
--  Constructing path-based diagrams  ----------------------
------------------------------------------------------------

-- | Enumeration of algorithms or \"rules\" for determining which
--   points lie in the interior of a (possibly self-intersecting)
--   path.
data FillRule
  = Winding  -- ^ Interior points are those with a nonzero
             --   /winding/ /number/.  See
             --   <http://en.wikipedia.org/wiki/Nonzero-rule>.
  | EvenOdd  -- ^ Interior points are those where a ray
             --   extended infinitely in a particular direction crosses
             --   the path an odd number of times. See
             --   <http://en.wikipedia.org/wiki/Even-odd_rule>.
    deriving (Show, Typeable, Eq, Ord)

instance AttributeClass FillRule
instance Semigroup FillRule where
  _ <> b = b

instance Default FillRule where
  def = Winding

-- | A record of options that control how a path is stroked.
--   @StrokeOpts@ is an instance of 'Default', so a @StrokeOpts@
--   records can be created using @'with' { ... }@ notation.
data StrokeOpts a
  = StrokeOpts
    { _vertexNames   :: [[a]]

    , _queryFillRule :: FillRule

    }

makeLensesWith (generateSignatures .~ False $ lensRules) ''StrokeOpts

-- | Atomic names that should be assigned to the vertices of the path so that
--   they can be referenced later.  If there are not enough names, the extra
--   vertices are not assigned names; if there are too many, the extra names
--   are ignored.  Note that this is a /list of lists/ of names, since paths
--   can consist of multiple trails.  The first list of names are assigned to
--   the vertices of the first trail, the second list to the second trail, and
--   so on.
--
--   The default value is the empty list.

vertexNames :: Lens (StrokeOpts a) (StrokeOpts a') [[a]] [[a']]

-- | The fill rule used for determining which points are inside the path.
--   The default is 'Winding'.  NOTE: for now, this only affects the resulting
--   diagram's 'Query', /not/ how it will be drawn!  To set the fill rule
--   determining how it is to be drawn, use the 'fillRule' function.
queryFillRule :: Lens' (StrokeOpts a) FillRule

instance Default (StrokeOpts a) where
  def = StrokeOpts
        { _vertexNames    = []
        , _queryFillRule = def
        }

-- | Convert a 'ToPath' object into a diagram.  The resulting diagram has the
--   names 0, 1, ... assigned to each of the path's vertices.
--
--   See also 'stroke'', which takes an extra options record allowing
--   its behaviour to be customized.
--
-- @
-- 'stroke' :: 'Path' 'V2' 'Double'                  -> 'Diagram' b
-- 'stroke' :: 'Located' ('Trail' 'V2' 'Double')       -> 'Diagram' b
-- 'stroke' :: 'Located' ('Trail'' 'Loop' 'V2' 'Double') -> 'Diagram' b
-- 'stroke' :: 'Located' ('Trail'' 'Line' 'V2' 'Double') -> 'Diagram' b
-- @
stroke :: (InSpace V2 n t, ToPath t, TypeableFloat n, Renderable (Path V2 n) b)
       => t -> QDiagram b V2 n Any
stroke = strokeP . toPath

-- | A variant of 'stroke' that takes an extra record of options to
--   customize its behaviour.  In particular:
--
--     * Names can be assigned to the path's vertices
--
--   'StrokeOpts' is an instance of 'Default', so @stroke' ('with' &
--   ... )@ syntax may be used.
stroke' :: (InSpace V2 n t, ToPath t, TypeableFloat n, Renderable (Path V2 n) b, IsName a)
       => StrokeOpts a -> t -> QDiagram b V2 n Any
stroke' opts = strokeP' opts . toPath

-- | 'stroke' specialised to 'Path'.
strokeP :: (TypeableFloat n, Renderable (Path V2 n) b)
        => Path V2 n -> QDiagram b V2 n Any
strokeP = strokeP' (def :: StrokeOpts ())

-- | 'stroke' specialised to 'Path'.
strokePath :: (TypeableFloat n, Renderable (Path V2 n) b)
        => Path V2 n -> QDiagram b V2 n Any
strokePath = strokeP

instance (TypeableFloat n, Renderable (Path V2 n) b)
    => TrailLike (QDiagram b V2 n Any) where
  trailLike = strokeP . trailLike

-- | 'stroke'' specialised to 'Path'.
strokeP' :: (TypeableFloat n, Renderable (Path V2 n) b, IsName a)
    => StrokeOpts a -> Path V2 n -> QDiagram b V2 n Any
strokeP' opts path
  | null (pLines ^. _Wrapped') = mkP pLoops
  | null (pLoops ^. _Wrapped') = mkP pLines
  | otherwise                  = mkP pLines <> mkP pLoops
  where
    (pLines,pLoops) = partitionPath (isLine . unLoc) path
    mkP p
      = mkQD (Prim p)
         (getEnvelope p)
         (getTrace p)
         (fromNames . concat $
           zipWith zip (opts^.vertexNames) ((map . map) subPoint (pathVertices p))
         )
         (Query $ Any . (runFillRule (opts^.queryFillRule)) p)

-- | 'stroke'' specialised to 'Path'.
strokePath' :: (TypeableFloat n, Renderable (Path V2 n) b, IsName a)
    => StrokeOpts a -> Path V2 n -> QDiagram b V2 n Any
strokePath' = strokeP'

-- | 'stroke' specialised to 'Trail'.
strokeTrail :: (TypeableFloat n, Renderable (Path V2 n) b)
            => Trail V2 n -> QDiagram b V2 n Any
strokeTrail = stroke . pathFromTrail

-- | 'stroke' specialised to 'Trail'.
strokeT :: (TypeableFloat n, Renderable (Path V2 n) b)
        => Trail V2 n -> QDiagram b V2 n Any
strokeT = strokeTrail

-- | A composition of 'stroke'' and 'pathFromTrail' for conveniently
--   converting a trail directly into a diagram.
strokeTrail' :: (TypeableFloat n, Renderable (Path V2 n) b, IsName a)
             => StrokeOpts a -> Trail V2 n -> QDiagram b V2 n Any
strokeTrail' opts = stroke' opts . pathFromTrail

-- | Deprecated synonym for 'strokeTrail''.
strokeT' :: (TypeableFloat n, Renderable (Path V2 n) b, IsName a)
         => StrokeOpts a -> Trail V2 n -> QDiagram b V2 n Any
strokeT' = strokeTrail'

-- | A composition of 'strokeT' and 'wrapLine' for conveniently
--   converting a line directly into a diagram.
strokeLine :: (TypeableFloat n, Renderable (Path V2 n) b)
           => Trail' Line V2 n -> QDiagram b V2 n Any
strokeLine = strokeT . wrapLine

-- | A composition of 'strokeT' and 'wrapLoop' for conveniently
--   converting a loop directly into a diagram.
strokeLoop :: (TypeableFloat n, Renderable (Path V2 n) b)
           => Trail' Loop V2 n -> QDiagram b V2 n Any
strokeLoop = strokeT . wrapLoop

-- | A convenience function for converting a @Located Trail@ directly
--   into a diagram; @strokeLocTrail = stroke . trailLike@.
strokeLocTrail :: (TypeableFloat n, Renderable (Path V2 n) b)
               => Located (Trail V2 n) -> QDiagram b V2 n Any
strokeLocTrail = strokeP . trailLike

-- | Deprecated synonym for 'strokeLocTrail'.
strokeLocT :: (TypeableFloat n, Renderable (Path V2 n) b)
           => Located (Trail V2 n) -> QDiagram b V2 n Any
strokeLocT = strokeLocTrail

-- | A convenience function for converting a @Located@ line directly
--   into a diagram; @strokeLocLine = stroke . trailLike . mapLoc wrapLine@.
strokeLocLine :: (TypeableFloat n, Renderable (Path V2 n) b)
              => Located (Trail' Line V2 n) -> QDiagram b V2 n Any
strokeLocLine = strokeP . trailLike . mapLoc wrapLine

-- | A convenience function for converting a @Located@ loop directly
--   into a diagram; @strokeLocLoop = stroke . trailLike . mapLoc wrapLoop@.
strokeLocLoop :: (TypeableFloat n, Renderable (Path V2 n) b)
              => Located (Trail' Loop V2 n) -> QDiagram b V2 n Any
strokeLocLoop = strokeP . trailLike . mapLoc wrapLoop

------------------------------------------------------------
--  Inside/outside testing
------------------------------------------------------------

runFillRule :: RealFloat n => FillRule -> Path V2 n -> Point V2 n -> Bool
runFillRule Winding = isInsideWinding
runFillRule EvenOdd = isInsideEvenOdd

-- | Extract the fill rule from a 'FillRuleA' attribute.
getFillRule :: FillRule -> FillRule
getFillRule = id

-- | Specify the fill rule that should be used for determining which
--   points are inside a path.
fillRule :: HasStyle a => FillRule -> a -> a
fillRule = applyAttr

-- | Lens onto the fill rule of a style.
_fillRule :: Lens' (Style V2 n) FillRule
_fillRule = atAttr . non def

-- | The sum of /signed/ crossings of a path as we travel in the
--   positive x direction from a given point.
--
--     - A point is filled according to the 'Winding' fill rule, if the
--       number of 'Crossings' is non-zero (see 'isInsideWinding').
--
--     - A point is filled according to the 'EvenOdd' fill rule, if the
--       number of 'Crossings' is odd (see 'isInsideEvenOdd').
--
--   This is the 'HasQuery' result for 'Path's, 'Located' 'Trail's and
--   'Located' 'Loops'.
--
-- @
-- 'sample' :: 'Path' 'V2' 'Double'                  -> 'Point' 'V2' 'Double' -> 'Crossings'
-- 'sample' :: 'Located' ('Trail' 'V2' 'Double')       -> 'Point' 'V2' 'Double' -> 'Crossings'
-- 'sample' :: 'Located' ('Trail'' 'Loop' 'V2' 'Double') -> 'Point' 'V2' 'Double' -> 'Crossings'
-- @
--
--   Note that 'Line's have no inside or outside, so don't contribute
--   crossings
newtype Crossings = Crossings Int
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance Semigroup Crossings where
  Crossings a <> Crossings b = Crossings (a + b)

instance Monoid Crossings where
  mempty  = Crossings 0
  mappend = (<>)

instance RealFloat n => HasQuery (Located (Trail V2 n)) Crossings where
  getQuery trail = Query $ \p -> trailCrossings p trail

instance RealFloat n => HasQuery (Located (Trail' l V2 n)) Crossings where
  getQuery trail' = getQuery (mapLoc Trail trail')

instance RealFloat n => HasQuery (Path V2 n) Crossings where
  getQuery = foldMapOf each getQuery

-- | Test whether the given point is inside the given path,
--   by testing whether the point's /winding number/ is nonzero. Note
--   that @False@ is /always/ returned for paths consisting of lines
--   (as opposed to loops), regardless of the winding number.
--
-- @
-- 'isInsideWinding' :: 'Path' 'V2' 'Double'                  -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideWinding' :: 'Located' ('Trail' 'V2' 'Double')       -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideWinding' :: 'Located' ('Trail'' 'Loop' 'V2' 'Double') -> 'Point' 'V2' 'Double' -> 'Bool'
-- @
isInsideWinding :: HasQuery t Crossings => t -> Point (V t) (N t) -> Bool
isInsideWinding t = (/= 0) . sample t

-- | Test whether the given point is inside the given path,
--   by testing whether a ray extending from the point in the positive
--   x direction crosses the path an even (outside) or odd (inside)
--   number of times.  Note that @False@ is /always/ returned for
--   paths consisting of lines (as opposed to loops), regardless of
--   the number of crossings.
--
-- @
-- 'isInsideEvenOdd' :: 'Path' 'V2' 'Double'                  -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideEvenOdd' :: 'Located' ('Trail' 'V2' 'Double')       -> 'Point' 'V2' 'Double' -> 'Bool'
-- 'isInsideEvenOdd' :: 'Located' ('Trail'' 'Loop' 'V2' 'Double') -> 'Point' 'V2' 'Double' -> 'Bool'
-- @
isInsideEvenOdd :: HasQuery t Crossings => t -> Point (V t) (N t) -> Bool
isInsideEvenOdd t = odd . sample t

-- | Compute the sum of signed crossings of a trail starting from the
--   given point in the positive x direction.
trailCrossings :: RealFloat n => Point V2 n -> Located (Trail V2 n) -> Crossings

  -- non-loop trails have no inside or outside, so don't contribute crossings
trailCrossings _ t | not (isLoop (unLoc t)) = 0

trailCrossings p@(unp2 -> (x,y)) tr
  = F.foldMap test $ fixTrail tr
  where
    test (FLinear a@(unp2 -> (_,ay)) b@(unp2 -> (_,by)))
      | ay <= y && by > y && isLeft a b > 0 =  1
      | by <= y && ay > y && isLeft a b < 0 = -1
      | otherwise                           =  0

    test c@(FCubic (P x1@(V2 _ x1y))
                   (P c1@(V2 _ c1y))
                   (P c2@(V2 _ c2y))
                   (P x2@(V2 _ x2y))
           ) =
        sum . map testT $ ts
      where ts = filter (liftA2 (&&) (>=0) (<=1))
               $ cubForm (-  x1y + 3*c1y - 3*c2y + x2y)
                         ( 3*x1y - 6*c1y + 3*c2y)
                         (-3*x1y + 3*c1y)
                         (x1y - y)
            testT t = let (unp2 -> (px,_)) = c `atParam` t
                      in  if px > x then signFromDerivAt t else 0
            signFromDerivAt t =
              let v =  (3*t*t) *^ ((-1)*^x1 ^+^ 3*^c1 ^-^ 3*^c2 ^+^ x2)
                   ^+^ (2*t)   *^ (3*^x1 ^-^ 6*^c1 ^+^ 3*^c2)
                   ^+^            ((-3)*^x1 ^+^ 3*^c1)
                  ang = v ^. _theta . rad
              in  case () of _ | 0      < ang && ang < tau/2 && t < 1 ->  1
                               | -tau/2 < ang && ang < 0     && t > 0 -> -1
                               | otherwise                            ->  0

    isLeft a b = cross2 (b .-. a) (p .-. a)

------------------------------------------------------------
--  Clipping  ----------------------------------------------
------------------------------------------------------------

-- | @Clip@ tracks the accumulated clipping paths applied to a
--   diagram.  Note that the semigroup structure on @Clip@ is list
--   concatenation, so applying multiple clipping paths is sensible.
--   The clipping region is the intersection of all the applied
--   clipping paths.
newtype Clip n = Clip [Path V2 n]
  deriving (Typeable, Semigroup)

makeWrapped ''Clip

instance Typeable n => AttributeClass (Clip n)

instance AsEmpty (Clip n) where
  _Empty = _Clip . _Empty

type instance V (Clip n) = V2
type instance N (Clip n) = n

instance (OrderedField n) => Transformable (Clip n) where
  transform t (Clip ps) = Clip (transform t ps)

-- | A point inside a clip if the point is in 'All' invididual clipping
--   paths.
instance RealFloat n => HasQuery (Clip n) All where
  getQuery (Clip paths) = Query $ \p ->
    F.foldMap (All . flip isInsideWinding p) paths

_Clip :: Iso (Clip n) (Clip n') [Path V2 n] [Path V2 n']
_Clip = _Wrapped

-- | Lens onto the Clip in a style. An empty list means no clipping.
_clip :: (Typeable n, OrderedField n) => Lens' (Style V2 n) [Path V2 n]
_clip = atTAttr . non' _Empty . _Clip

-- | Clip a diagram by the given path:
--
--   * Only the parts of the diagram which lie in the interior of the
--     path will be drawn.
--
--   * The envelope of the diagram is unaffected.
clipBy :: (HasStyle a, V a ~ V2, N a ~ n, TypeableFloat n) => Path V2 n -> a -> a
clipBy = applyTAttr . Clip . (:[])

-- | Clip a diagram to the given path setting its envelope to the
--   pointwise minimum of the envelopes of the diagram and path. The
--   trace consists of those parts of the original diagram's trace
--   which fall within the clipping path, or parts of the path's trace
--   within the original diagram.
clipTo :: TypeableFloat n
  => Path V2 n -> QDiagram b V2 n Any -> QDiagram b V2 n Any
clipTo p d = setTrace intersectionTrace . toEnvelope $ clipBy p d
  where
    envP = appEnvelope . getEnvelope $ p
    envD = appEnvelope . getEnvelope $ d
    toEnvelope = case (envP, envD) of
      (Just eP, Just eD) -> setEnvelope . mkEnvelope $ \v -> min (eP v) (eD v)
      (_, _)             -> id
    intersectionTrace = Trace traceIntersections
    traceIntersections pt v =
        -- on boundary of d, inside p
        onSortedList (filter pInside) (appTrace (getTrace d) pt v) <>
        -- or on boundary of p, inside d
        onSortedList (filter dInside) (appTrace (getTrace p) pt v) where
          newPt dist = pt .+^ v ^* dist
          pInside dDist = runFillRule Winding p (newPt dDist)
          dInside pDist = getAny . sample d $ newPt pDist

-- | Clip a diagram to the clip path taking the envelope and trace of the clip
--   path.
clipped :: TypeableFloat n
  => Path V2 n -> QDiagram b V2 n Any -> QDiagram b V2 n Any
clipped p = withTrace p . withEnvelope p . clipBy p

------------------------------------------------------------
--  Intersections  -----------------------------------------
------------------------------------------------------------

-- | Find the intersect points of two objects that can be converted to a path.
intersectPoints :: (InSpace V2 n t, SameSpace t s, ToPath t, ToPath s, OrderedField n)
  => t -> s -> [P2 n]
intersectPoints = intersectPoints' 1e-8

-- | Find the intersect points of two objects that can be converted to a path
--   within the given tolerance.
intersectPoints' :: (InSpace V2 n t, SameSpace t s, ToPath t, ToPath s, OrderedField n)
  => n -> t -> s -> [P2 n]
intersectPoints' eps t s = intersectPointsP' eps (toPath t) (toPath s)

-- | Compute the intersect points between two paths.
intersectPointsP :: OrderedField n => Path V2 n -> Path V2 n -> [P2 n]
intersectPointsP = intersectPointsP' 1e-8

-- | Compute the intersect points between two paths within given tolerance.
intersectPointsP' :: OrderedField n => n -> Path V2 n -> Path V2 n -> [P2 n]
intersectPointsP' eps as bs = do
  a <- pathTrails as
  b <- pathTrails bs
  intersectPointsT' eps a b

-- | Compute the intersect points between two located trails.
intersectPointsT :: OrderedField n => Located (Trail V2 n) -> Located (Trail V2 n) -> [P2 n]
intersectPointsT = intersectPointsT' 1e-8

-- | Compute the intersect points between two located trails within the given
--   tolerance.
intersectPointsT' :: OrderedField n => n -> Located (Trail V2 n) -> Located (Trail V2 n) -> [P2 n]
intersectPointsT' eps as bs = do
  a <- fixTrail as
  b <- fixTrail bs
  intersectPointsS' eps a b
