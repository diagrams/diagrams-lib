{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Model
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Tools for visualizing diagrams' internal model: local origins,
-- envelopes, /etc./
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Model
       ( -- * Showing the local origin
         showOrigin
       , showOrigin'
       , OriginOpts(..), oColor, oMeasure

         -- * Approximating the envelope
       , showEnvelope
       , showEnvelope'
       , EnvelopeOpts(..), eColor, eLineWidth, ePoints

         -- * Approximating the trace
       , showTrace
       , showTrace'
       , TraceOpts(..), tColor, tScale, tMinSize, tPoints

         -- * Show labels
       , showLabels

         -- * Path control points
       , illustratePath, illustratePath'
       , IllustratePathOpts (..)
       , endPtStyle, endPtVisible, endPtMeasure
       , ctrlPtStyle, ctrlPtVisible, ctrlPtMeasure
       , ctrlLnStyle
       ) where

import           Control.Arrow            (second)
import           Control.Lens             (makeLenses, (^.))
import           Data.Colour              (Colour)
import           Data.Colour.Names
import           Data.Default.Class
import qualified Data.Map                 as M
import           Data.Maybe               (catMaybes)
import           Data.Semigroup
import           Data.Foldable            (foldMap)
import           Data.Functor

import           Diagrams.Attributes
import           Diagrams.Combinators     (atPoints)
import           Diagrams.Core
import           Diagrams.Segment
import           Diagrams.TrailLike
import           Diagrams.CubicSpline
import           Diagrams.Measured
import           Diagrams.Path
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Ellipse
import           Diagrams.TwoD.Path      (strokeP)
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Transform (rotateBy)
import           Diagrams.TwoD.Types     (V2 (..))
import           Diagrams.TwoD.Vector    (unitX)
import           Diagrams.Util

import           Linear.Affine
import           Linear.Vector

------------------------------------------------------------
-- Marking the origin
------------------------------------------------------------

data OriginOpts n = OriginOpts
  { _oColor   :: Colour Double
  , _oMeasure :: Measure n
  }

makeLenses ''OriginOpts

instance (Fractional n, Ord n) => Default (OriginOpts n) where
  def = OriginOpts red (normalized 0.05 `atMost` output 20)

-- | Mark the origin of a diagram by placing a red dot 1/50th its size.
showOrigin :: (TypeableFloat n, Renderable (Path V2 n) b, Monoid' m)
           => QDiagram b V2 n m -> QDiagram b V2 n m
showOrigin = showOrigin' def

-- | Mark the origin of a diagram, with control over colour and scale
-- of marker dot.
showOrigin' :: (TypeableFloat n, Renderable (Path V2 n) b, Monoid' m)
            => OriginOpts n -> QDiagram b V2 n m -> QDiagram b V2 n m
showOrigin' oo d = o <> d
  where
    -- can't use circle alone because ghc doesn't know which monoid to
    -- pick for the intermediate diagram
    o = measuredDiagram (strokeP . circle <$> (oo^.oMeasure))
          # (mempty <$)
          # fc (oo^.oColor)
          # lw none

------------------------------------------------------------
-- Approximating the envelope
------------------------------------------------------------

data EnvelopeOpts n = EnvelopeOpts
  { _eColor     :: Colour Double
  , _eLineWidth :: Measure n
  , _ePoints    :: Int
  }

makeLenses ''EnvelopeOpts

instance OrderedField n => Default (EnvelopeOpts n) where
  def = EnvelopeOpts red medium 32

-- | Mark the envelope with an approximating cubic spline with control
--   over the color, line width and number of points.
showEnvelope' :: (Enum n, TypeableFloat n, Renderable (Path V2 n) b)
              => EnvelopeOpts n -> QDiagram b V2 n Any -> QDiagram b V2 n Any
showEnvelope' opts d = cubicSpline True pts # lc (opts^.eColor)
                                            # lw w <> d
  where
    pts = catMaybes [envelopePMay v d | v <- map (`rotateBy` unitX) [0,inc..top]]
    w   = opts ^. eLineWidth
    inc = 1 / fromIntegral (opts^.ePoints)
    top = 1 - inc

-- | Mark the envelope with an approximating cubic spline
--   using 32 points, medium line width and red line color.
showEnvelope :: (Enum n, TypeableFloat n, Renderable (Path V2 n) b)
             => QDiagram b V2 n Any -> QDiagram b V2 n Any
showEnvelope = showEnvelope' def

------------------------------------------------------------
-- Approximating the trace
------------------------------------------------------------

data TraceOpts n = TraceOpts
  { _tColor   :: Colour Double
  , _tScale   :: n
  , _tMinSize :: n
  , _tPoints  :: Int
  }

makeLenses ''TraceOpts

instance Floating n => Default (TraceOpts n) where
  def = TraceOpts red (1/100) 0.001 64

-- | Mark the trace of a diagram, with control over colour and scale
-- of marker dot and the number of points on the trace.
showTrace' :: (Enum n, TypeableFloat n, Renderable (Path V2 n) b)
          => TraceOpts n -> QDiagram b V2 n Any -> QDiagram b V2 n Any
showTrace' opts d =  atPoints ps (repeat pt) <> d
  where
    ps = concatMap p ts
    ts = zip rs vs
    p (r, v) = [origin .+^ (s *^ v) | s <- r]
    vs = map (`rotateBy` unitX) [0, inc..top]
    rs = [getSortedList $ (appTrace . getTrace) d origin v | v <- vs]
    pt = circle sz # fc (opts^.tColor) # lw none
    V2 w h = opts^.tScale *^ size d
    sz     = maximum [w, h, opts^.tMinSize]
    inc = 1 / fromIntegral (opts^.tPoints)
    top = 1 - inc

-- | Mark the trace of a diagram by placing 64 red dots 1/100th its size
--   along the trace.
showTrace :: (Enum n, TypeableFloat n, Renderable (Path V2 n) b)
          => QDiagram b V2 n Any -> QDiagram b V2 n Any
showTrace = showTrace' def

------------------------------------------------------------
-- Labeling named points
------------------------------------------------------------

showLabels :: (TypeableFloat n, Renderable (Text n) b, Semigroup m)
           => QDiagram b V2 n m -> QDiagram b V2 n Any
showLabels d =
             ( mconcat
             . map (\(n,p) -> text (show n) # translate (p .-. origin))
             . concatMap (\(n,ps) -> zip (repeat n) ps)
             . (map . second . map) location
             . M.assocs
             $ m
             ) <>
             fmap (const (Any False)) d
  where
    SubMap m = d^.subMap

------------------------------------------------------------
-- Labeling path control points
------------------------------------------------------------

data IllustratePathOpts n =
  IllustratePathOpts
    { _endPtStyle    :: Style V2 n
    , _endPtMeasure  :: Measure n
    , _endPtVisible  :: Bool
    , _ctrlPtStyle   :: Style V2 n
    , _ctrlLnStyle   :: Style V2 n
    , _ctrlPtMeasure :: Measure n
    , _ctrlPtVisible :: Bool
    }

makeLenses ''IllustratePathOpts

instance TypeableFloat n => Default (IllustratePathOpts n) where
  def = IllustratePathOpts
    { _endPtStyle    = mempty # fc orange
    , _endPtMeasure  = output 4
    , _endPtVisible  = True
    , _ctrlPtStyle   = mempty # fc dodgerblue # lw ultraThin
    , _ctrlLnStyle   = mempty # lw thin # fc grey
    , _ctrlPtMeasure = output 3
    , _ctrlPtVisible = True
    }

drawSegs
  :: (TypeableFloat n, Renderable (Path V2 n) b)
  => IllustratePathOpts n -> [FixedSegment V2 n] -> QDiagram b V2 n Any
drawSegs opts segs
  = foldMap (place endPt) ends             # applyStyle (opts^.endPtStyle)
 <> foldMap (place ctrlPt) (map snd ctrls) # applyStyle (opts^.ctrlPtStyle)
 <> foldMap (uncurry (~~)) ctrls           # applyStyle (opts^.ctrlLnStyle)
  where
    endPt  = measuredDiagram $ circle <$> opts^.endPtMeasure
    ctrlPt = measuredDiagram $ circle <$> opts^.ctrlPtMeasure
    --
    ends  | opts^.endPtVisible  = map getEnd segs
          | otherwise           = []
    ctrls | opts^.ctrlPtVisible = foldMap getCtrl segs
          | otherwise           = []
    --
    getEnd (FLinear _ p1)        = p1
    getEnd (FCubic _ _ _ p3)     = p3
    getCtrl (FCubic p0 p1 p2 p3) = [(p0, p1), (p3,p2)]
    getCtrl _                    = []

illustratePath :: (Renderable (Path V2 n) b, TypeableFloat n)
               => Path V2 n -> QDiagram b V2 n Any
illustratePath = illustratePath' def

illustratePath' :: (Renderable (Path V2 n) b, TypeableFloat n)
                => IllustratePathOpts n -> Path V2 n -> QDiagram b V2 n Any
illustratePath' opts p
  = strokeP p <> (drawSegs opts . concat . fixPath . toPath) p

