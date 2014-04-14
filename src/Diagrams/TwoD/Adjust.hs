{-# LANGUAGE Rank2Types #-}

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
    (
      setDefault2DAttributes
    , adjustDiaSize2D
    , adjustDia2D
    ) where

import           Diagrams.Attributes      (lineCap, lineColorA, lineJoin,
                                           lineMiterLimitA)
import           Diagrams.Core
import           Diagrams.TwoD.Attributes (lineWidthA)
import           Diagrams.TwoD.Size       (SizeSpec2D (..), center2D,
                                           requiredScale, size2D)
import           Diagrams.TwoD.Text       (fontSizeA)
import           Diagrams.TwoD.Types      (R2, T2, p2)
import           Diagrams.Util            (( # ))

import           Control.Lens             (Lens', (&), (.~), (^.))
import           Data.AffineSpace         ((.-.))
import           Data.Default.Class
import           Data.Semigroup

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
setDefault2DAttributes :: Semigroup m => QDiagram b R2 m -> QDiagram b R2 m
setDefault2DAttributes d = d # lineWidthA def # lineColorA def # fontSizeA def
                             # lineCap def # lineJoin def # lineMiterLimitA def
                             # lineTextureA def

-- | Adjust the size and position of a 2D diagram to fit within the
--   requested size. The first argument is a lens into the output
--   size contained in the rendering options.  Returns an updated
--   options record, any transformation applied to the diagram (the
--   inverse of which can be used, say, to translate output/device
--   coordinates back into local diagram coordinates), and the
--   modified diagram itself.
adjustDiaSize2D :: Monoid' m
                => Lens' (Options b R2) SizeSpec2D
                -> b -> Options b R2 -> QDiagram b R2 m
                -> (Options b R2, T2, QDiagram b R2 m)
adjustDiaSize2D szL _ opts d =
  ( case spec of
     Dims _ _ -> opts
     _        -> opts & szL .~ (uncurry Dims . scale s $ size)
  , adjustT
  , d # transform adjustT
  )
  where spec = opts ^. szL
        size = size2D d
        s    = requiredScale spec size
        finalSz = case spec of
                    Dims w h -> (w,h)
                    _        -> scale s size
        tr = (0.5 *. p2 finalSz) .-. (s *. center2D d)
        adjustT = translation tr <> scaling s

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
adjustDia2D :: Monoid' m
            => Lens' (Options b R2) SizeSpec2D
            -> b -> Options b R2 -> QDiagram b R2 m
            -> (Options b R2, T2, QDiagram b R2 m)
adjustDia2D szL b opts d
  = adjustDiaSize2D szL b opts (d # setDefault2DAttributes)

