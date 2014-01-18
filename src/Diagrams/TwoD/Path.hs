{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
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
-- intersection and union.  They also have a trace, whereas paths in
-- higher dimensions do not.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Path
       ( -- * Constructing path-based diagrams

         stroke, stroke', strokeTrail, strokeT, strokeTrail', strokeT'
       , strokeLine, strokeLoop
       , strokeLocTrail, strokeLocT, strokeLocLine, strokeLocLoop

         -- ** Stroke options

       , FillRule(..)
       , FillRuleA(..), getFillRule, fillRule
       , StrokeOpts(..), vertexNames, queryFillRule

         -- ** Inside/outside testing

       , isInsideWinding, isInsideEvenOdd

         -- * Clipping

       , Clip(..), clipBy
       ) where

import           Control.Applicative   (liftA2)
import           Control.Lens          ( makeWrapped, makeLensesWith, (.~), (^.)
                                       , generateSignatures, lensRules, op
                                       , Lens, Lens', unwrapped)
import qualified Data.Foldable         as F
import           Data.Semigroup
import           Data.Typeable

import           Data.AffineSpace
import           Data.Default.Class
import           Data.VectorSpace

import           Data.Colour           (transparent)

import           Diagrams.Coordinates
import           Diagrams.Core
import           Diagrams.Located      (Located, mapLoc, unLoc)
import           Diagrams.Parametric
import           Diagrams.Path
import           Diagrams.Segment
import           Diagrams.Solve
import           Diagrams.Trail
import           Diagrams.TrailLike
import           Diagrams.Attributes   (fcA)
import           Diagrams.TwoD.Segment ()
import           Diagrams.TwoD.Types
import           Diagrams.Util         (tau)

------------------------------------------------------------
--  Trail and path traces  ---------------------------------
------------------------------------------------------------

-- Only 2D trails and paths have a trace.

-- XXX can the efficiency of this be improved?  See the comment in
-- Diagrams.Path on the Enveloped instance for Trail.
instance Traced (Trail R2) where
  getTrace = withLine $
      foldr
        (\seg bds -> moveOriginBy (negateV . atEnd $ seg) bds <> getTrace seg)
        mempty
    . lineSegments

instance Traced (Path R2) where
  getTrace = F.foldMap getTrace . op Path

------------------------------------------------------------
--  Constructing path-based diagrams  ----------------------
------------------------------------------------------------

-- | Enumeration of algorithms or \"rules\" for determining which
--   points lie in the interior of a (possibly self-intersecting)
--   closed path.
data FillRule = Winding  -- ^ Interior points are those with a nonzero
                         --   /winding/ /number/.  See
                         --   <http://en.wikipedia.org/wiki/Nonzero-rule>.
              | EvenOdd  -- ^ Interior points are those where a ray
                         --   extended infinitely in a particular
                         --   direction crosses the path an odd number
                         --   of times. See
                         --   <http://en.wikipedia.org/wiki/Even-odd_rule>.
    deriving (Eq, Show)

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

vertexNames :: forall a a'. Lens (StrokeOpts a) (StrokeOpts a') [[a]] [[a']]

-- | The fill rule used for determining which points are inside the path.
--   The default is 'Winding'.  NOTE: for now, this only affects the resulting
--   diagram's 'Query', /not/ how it will be drawn!  To set the fill rule
--   determining how it is to be drawn, use the 'fillRule' function.
queryFillRule :: forall a. Lens' (StrokeOpts a) FillRule


instance Default (StrokeOpts a) where
  def = StrokeOpts
        { _vertexNames    = []
        , _queryFillRule = def
        }

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

instance Renderable (Path R2) b => TrailLike (QDiagram b R2 Any) where
  trailLike = stroke . trailLike

-- | A variant of 'stroke' that takes an extra record of options to
--   customize its behavior.  In particular:
--
--     * Names can be assigned to the path's vertices
--
--   'StrokeOpts' is an instance of 'Default', so @stroke' 'with' {
--   ... }@ syntax may be used.
stroke' :: (Renderable (Path R2) b, IsName a) => StrokeOpts a -> Path R2 -> Diagram b R2
stroke' opts path
  | null (pLines ^. unwrapped) =  mkP pLoops
  | null (pLoops ^. unwrapped) =  fcA transparent $ mkP pLines
  | otherwise                  = (fcA transparent $ mkP pLines) <> mkP pLoops
  where
    (pLines,pLoops) = partitionPath (isLine . unLoc) path
    mkP p
      = mkQD (Prim p)
         (getEnvelope p)
         (getTrace p)
         (fromNames . concat $
           zipWith zip (opts^.vertexNames) ((map . map) subPoint (pathVertices p))
         )
         (Query $ Any . flip (runFillRule (opts^.queryFillRule)) p)


-- | A composition of 'stroke' and 'pathFromTrail' for conveniently
--   converting a trail directly into a diagram.
--
--   Note that a bug in GHC 7.0.1 causes a context stack overflow when
--   inferring the type of 'stroke' and hence of @strokeTrail@ as well.
--   The solution is to give a type signature to expressions involving
--   @strokeTrail@, or (recommended) upgrade GHC (the bug is fixed in 7.0.2
--   onwards).
strokeTrail :: (Renderable (Path R2) b) => Trail R2 -> Diagram b R2
strokeTrail = stroke . pathFromTrail

-- | Deprecated synonym for 'strokeTrail'.
strokeT :: (Renderable (Path R2) b) => Trail R2 -> Diagram b R2
strokeT = strokeTrail

-- | A composition of 'stroke'' and 'pathFromTrail' for conveniently
--   converting a trail directly into a diagram.
strokeTrail' :: (Renderable (Path R2) b, IsName a)
             => StrokeOpts a -> Trail R2 -> Diagram b R2
strokeTrail' opts = stroke' opts . pathFromTrail

-- | Deprecated synonym for 'strokeTrail''.
strokeT' :: (Renderable (Path R2) b, IsName a)
         => StrokeOpts a -> Trail R2 -> Diagram b R2
strokeT' = strokeTrail'

-- | A composition of 'strokeT' and 'wrapLine' for conveniently
--   converting a line directly into a diagram.
strokeLine :: (Renderable (Path R2) b) => Trail' Line R2 -> Diagram b R2
strokeLine = strokeT . wrapLine

-- | A composition of 'strokeT' and 'wrapLoop' for conveniently
--   converting a loop directly into a diagram.
strokeLoop :: (Renderable (Path R2) b) => Trail' Loop R2 -> Diagram b R2
strokeLoop = strokeT . wrapLoop

-- | A convenience function for converting a @Located Trail@ directly
--   into a diagram; @strokeLocTrail = stroke . trailLike@.
strokeLocTrail :: (Renderable (Path R2) b) => Located (Trail R2) -> Diagram b R2
strokeLocTrail = stroke . trailLike

-- | Deprecated synonym for 'strokeLocTrail'.
strokeLocT :: (Renderable (Path R2) b) => Located (Trail R2) -> Diagram b R2
strokeLocT = strokeLocTrail

-- | A convenience function for converting a @Located@ line directly
--   into a diagram; @strokeLocLine = stroke . trailLike . mapLoc wrapLine@.
strokeLocLine :: (Renderable (Path R2) b) => Located (Trail' Line R2) -> Diagram b R2
strokeLocLine = stroke . trailLike . mapLoc wrapLine

-- | A convenience function for converting a @Located@ loop directly
--   into a diagram; @strokeLocLoop = stroke . trailLike . mapLoc wrapLoop@.
strokeLocLoop :: (Renderable (Path R2) b) => Located (Trail' Loop R2) -> Diagram b R2
strokeLocLoop = stroke . trailLike . mapLoc wrapLoop

------------------------------------------------------------
--  Inside/outside testing
------------------------------------------------------------



runFillRule :: FillRule -> P2 -> Path R2 -> Bool
runFillRule Winding = isInsideWinding
runFillRule EvenOdd = isInsideEvenOdd

newtype FillRuleA = FillRuleA (Last FillRule)
  deriving (Typeable, Semigroup, Show)
instance AttributeClass FillRuleA

instance Default FillRuleA where
  def = FillRuleA $ Last $ def

-- | Extract the fill rule from a 'FillRuleA' attribute.
getFillRule :: FillRuleA -> FillRule
getFillRule (FillRuleA (Last r)) = r

-- | Specify the fill rule that should be used for determining which
--   points are inside a path.
fillRule :: HasStyle a => FillRule -> a -> a
fillRule = applyAttr . FillRuleA . Last

cross :: R2 -> R2 -> Double
cross (coords -> x :& y) (coords -> x' :& y') = x * y' - y * x'

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

-- | Compute the sum of /signed/ crossings of a path as we travel in the
--   positive x direction from a given point.
crossings :: P2 -> Path R2 -> Int
crossings p = F.sum . map (trailCrossings p) . op Path

-- | Compute the sum of signed crossings of a trail starting from the
--   given point in the positive x direction.
trailCrossings :: P2 -> Located (Trail R2) -> Int

  -- non-loop trails have no inside or outside, so don't contribute crossings
trailCrossings _ t | not (isLoop (unLoc t)) = 0

trailCrossings p@(unp2 -> (x,y)) tr
  = sum . map test $ fixTrail tr
  where
    test (FLinear a@(unp2 -> (_,ay)) b@(unp2 -> (_,by)))
      | ay <= y && by > y && isLeft a b > 0 =  1
      | by <= y && ay > y && isLeft a b < 0 = -1
      | otherwise                           =  0

    test c@(FCubic (unp2 -> x1@(_,x1y))
                   (unp2 -> c1@(_,c1y))
                   (unp2 -> c2@(_,c2y))
                   (unp2 -> x2@(_,x2y))
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
              let (dx,dy) = (3*t*t) *^ ((-1)*^x1 ^+^ 3*^c1 ^-^ 3*^c2 ^+^ x2)
                        ^+^ (2*t)   *^ (3*^x1 ^-^ 6*^c1 ^+^ 3*^c2)
                        ^+^            ((-3)*^x1 ^+^ 3*^c1)
                  ang = atan2 dy dx
              in  case () of _ | 0      < ang && ang < tau/2 && t < 1 ->  1
                               | -tau/2 < ang && ang < 0     && t > 0 -> -1
                               | otherwise                            ->  0

    isLeft a b = cross (b .-. a) (p .-. a)

------------------------------------------------------------
--  Clipping  ----------------------------------------------
------------------------------------------------------------

-- | @Clip@ tracks the accumulated clipping paths applied to a
--   diagram.  Note that the semigroup structure on @Clip@ is list
--   concatenation, so applying multiple clipping paths is sensible.
--   The clipping region is the intersection of all the applied
--   clipping paths.
newtype Clip = Clip [Path R2]
  deriving (Typeable, Semigroup)

makeWrapped ''Clip

instance AttributeClass Clip

type instance V Clip = R2

instance Transformable Clip where
  transform t (Clip ps) = Clip (transform t ps)

-- | Clip a diagram by the given path:
--
--   * Only the parts of the diagram which lie in the interior of the
--     path will be drawn.
--
--   * The envelope of the diagram is unaffected.
clipBy :: (HasStyle a, V a ~ R2) => Path R2 -> a -> a
clipBy = applyTAttr . Clip . (:[])

-- XXX Should include a 'clipTo' function which clips a diagram AND
-- restricts its envelope.  It will have to take a *pointwise minimum*
-- of the diagram's current envelope and the path's envelope.  Not
-- sure of the best way to do this at the moment.
