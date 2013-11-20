-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Palettes
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Utility functions to choose pleasing colors for creating diagrams.
-- d3 Colors from https:\/\/github.com\/mbostock\/d3\/wiki\/Ordinal-Scales.
--
-----------------------------------------------------------------------------

module Diagrams.Palettes
       (
         d3Single, d3Pair, d3Quad
       ) where

import Data.Colour
import Data.Colour.SRGB (sRGB24read)

d3c10 :: [Colour Double]
d3c10 = map sRGB24read [ "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"
                       , "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"]

-- | Choose from one of 10 contrasting colors (0-9) borrowed from mbostock's d3.
--
-- <<diagrams/src_Diagrams_Palettes_singles.svg#diagram=singles&width=400>>
--
-- > singles      = hcat (colorBar (const d3Single) 0)
-- > colorBar m k = [square 1 # fc (m k i) | i <- [0..9]]
d3Single :: Int ->  Colour Double
d3Single n = d3c10 !! (n `mod` 10)

d3c20 :: [Colour Double]
d3c20 = map sRGB24read [ "#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#2ca02c"
                       , "#98df8a", "#d62728", "#ff9896", "#9467bd", "#c5b0d5"
                       , "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", "#7f7f7f"
                       , "#c7c7c7", "#bcbd22", "#dbdb8d", "#17becf", "#9edae5"]

-- | Choose 0 for dark and 1 for light for each pair of 10 sets of contrasting
--   colors (0-9) borrowed from mbostock's d3.
--
-- <<diagrams/src_Diagrams_Palettes_pairs.svg#diagram=pairs&width=400>>
--
-- > pairs      = vcat $ map (hcat . colorBar d3Pair) [0, 1]
d3Pair :: Int -> Int -> Colour Double
d3Pair k n = d3c20 !! (((2*n) `mod` 20) + (k `mod` 2))

d3c20bc :: [Colour Double]
d3c20bc = map sRGB24read [ "#393b79", "#5254a3", "#6b6ecf", "#9c9ede", "#637939"
                         , "#8ca252", "#b5cf6b", "#cedb9c", "#8c6d31", "#bd9e39"
                         , "#e7ba52", "#e7cb94", "#843c39", "#ad494a", "#d6616b"
                         , "#e7969c", "#7b4173", "#a55194", "#ce6dbd", "#de9ed6"
                         , "#3182bd", "#6baed6", "#9ecae1", "#c6dbef", "#e6550d"
                         , "#fd8d3c", "#fdae6b", "#fdd0a2", "#31a354", "#74c476"
                         , "#a1d99b", "#c7e9c0", "#756bb1", "#9e9ac8", "#bcbddc"
                         , "#dadaeb", "#636363", "#969696", "#bdbdbd", "#d9d9d9"]

-- | Choose from 4 levels of darkness - 0 for darkest, 3 - for lightest. From
--   10 quadruples of contrasting colors (0-9) borrowed from mbostock's d3.
--
-- <<diagrams/src_Diagrams_Palettes_quads.svg#diagram=quads&width=400>>
--
-- > quads      = vcat $ map (hcat . colorBar d3Quad) [0..3]
d3Quad :: Int -> Int -> Colour Double
d3Quad k n =d3c20bc !! (((4*n) `mod` 40) + (k `mod` 4))


