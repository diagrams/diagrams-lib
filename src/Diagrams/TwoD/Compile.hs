-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Compile
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Functions to rewrite the RTree form of diagrams during backend rendering.
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Compile
       ( toOutput

       ) where

import           Diagrams.Core
import           Diagrams.Core.Types      (RTree)
import           Diagrams.TwoD.Arrow
import           Diagrams.TwoD.Attributes
import           Diagrams.TwoD.Size       (SizeSpec2D, sizePair)
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Types      (R2)

-- | Take a getter, a setter, and two scaling terms, return a function
-- that rewrites the given Measure-containing Attribute into Output
-- units.  The scaling terms are from Normalized to Output and from
-- Global to Output.
modifyStyle :: AttributeClass a =>
                 (a -> Measure Double) ->
                 (Measure Double -> Style R2 -> Style R2) ->
                 SizeSpec2D -> Double ->
                 Style R2 -> Style R2
modifyStyle get set outputSize gs sty = case getAttr sty of
    Nothing -> sty
    Just a  -> case get a of
        Output _     -> sty
        Normalized t -> set (Output $ sqrt (w*h) * t) sty where
          (w,h) = sizePair outputSize

        -- Note: we assume here that this function is being called on
        -- values in an RTree; conversion to RTree involves pushing
        -- transformations down to the leaves, and transforming any
        -- styles encountered along the way.  In particular this will
        -- scale any 'Local' values; hence any 'Local' value
        -- encountered here must already be in Output units.
        Local t      -> set (Output t) sty

        Global t     -> set (Output $ gs * t) sty

-- | Convert all of the @LineWidth@ attributes in an @RTree@ to output
--   units. 'w' and 'h' are the width and height of the final diagram.
--   The scaling factor is the geometric mean of 'h' and 'w'.
toOutput :: SizeSpec2D -> Double -> Style R2 -> Style R2
toOutput ns gs = modifyStyle getLineWidth setLineWidth ns gs .
                 modifyStyle getHeadSize setHeadSize ns gs .
                 modifyStyle getTailSize setTailSize ns gs .
                 modifyStyle getFontSize setFontSize ns gs
