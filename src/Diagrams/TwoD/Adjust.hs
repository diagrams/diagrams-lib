{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeFamilies     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Adjust
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A default diagram-adjustment implementation for two-dimensional
-- diagrams, useful for backend implementors.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Adjust
    ( setDefault2DAttributes
    , adjustSize
    , adjustDia2D
    ) where

import           Diagrams.Attributes
import           Diagrams.Core
import           Diagrams.Core.Envelope
import           Diagrams.TwoD.Attributes (lineTextureA)
import           Diagrams.TwoD.Text       (fontSizeM)
import           Diagrams.TwoD.Types
import           Diagrams.Util            (( # ))
import           Diagrams.Size

import           Control.Lens             (Lens', (^.),  set, allOf, folded)
import           Data.Default.Class
import           Data.Semigroup
import           Data.Maybe

import           Linear.Vector

-- | Set default attributes of a 2D diagram (in case they have not
--   been set):
--
--       * Line width 0.01
--
--       * Line color black
--
--       * Font size 1
--
--       * Line cap LineCapButt
--
--       * line join miter
--
--       * Miter limit 10
setDefault2DAttributes :: (TypeableFloat n, Semigroup m)
  => QDiagram b V2 n m -> QDiagram b V2 n m
setDefault2DAttributes d
  = d # lineWidthM def
      # lineTextureA def
      # fontSizeM def
      # lineCap def
      # lineJoin def
      # lineMiterLimitA def


-- | Adjust the size and position of a 2D diagram to fit within the
--   requested size. The first argument is a lens into the output
--   size contained in the rendering options.  Returns an updated
--   options record, any transformation applied to the diagram (the
--   inverse of which can be used, say, to translate output/device
--   coordinates back into local diagram coordinates), and the
--   modified diagram itself.
adjustSize :: (TypeableFloat n, Monoid' m)
                => Lens' (Options b V2 n) (SizeSpec V2 n)
                -> b -> Options b V2 n -> QDiagram b V2 n m
                -> (Options b V2 n, Transformation V2 n, QDiagram b V2 n m)
adjustSize szL _ opts d = (set szL sz' opts, t, d # transform t)
  where
    spec = opts ^. szL

    s   = requiredScale spec sz
    sz  = size d
    sz' = if allOf folded isJust (getSpec spec)
            then spec
            else dims $ s *^ sz

    -- vector from the origin to lower corner of envelope
    v  = fmap (flip envelopeS d . negated) eye

    -- transform by moving lower corner to origin and scale
    t  = scaling s <> translation v

-- | @adjustDia2D@ provides a useful default implementation of
--   the 'adjustDia' method from the 'Backend' type class.
--
--   As its first argument it requires a lens into the output size
--   contained in the rendering options.
--
--   It then performs the following adjustments:
--
--   * Set default attributes (see 'setDefault2DAttributes')
--
--   * Scale and translate the diagram to fit within the requested
--     size (see 'adjustDiaSize2D')
--
--   It returns an updated options record, any transformation applied
--   to the diagram (the inverse of which can be used, say, to
--   translate output/device coordinates back into local diagram
--   coordinates), and the modified diagram itself.
adjustDia2D :: (TypeableFloat n, Monoid' m)
            => Lens' (Options b V2 n) (SizeSpec V2 n)
            -> b -> Options b V2 n -> QDiagram b V2 n m
            -> (Options b V2 n, Transformation V2 n, QDiagram b V2 n m)
adjustDia2D szL b opts d
  = adjustSize szL b opts (d # setDefault2DAttributes)

