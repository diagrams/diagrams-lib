{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
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
       , showEnvelope
       , showEnvelope'
       , EnvelopeOpts(..), eColor, eLineWidth, ePoints
       , showTrace
       , showTrace'
       , TraceOpts(..), tColor, tScale, tMinSize, tPoints
       , showLabels
       ) where

import           Control.Arrow            (second)
import           Control.Lens             (makeLenses, (^.))
import           Data.Colour              (Colour)
import           Data.Colour.Names
import           Data.Default.Class
import qualified Data.Map                 as M
import           Data.Maybe               (catMaybes)
import           Data.Semigroup
import           Data.Functor

import           Diagrams.Attributes
import           Diagrams.Combinators     (atPoints)
import           Diagrams.Core
import           Diagrams.CubicSpline
import           Diagrams.Measured
import           Diagrams.Path
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Ellipse
import           Diagrams.TwoD.Path      (strokeP)
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Transform (rotateBy)
import           Diagrams.TwoD.Types
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
