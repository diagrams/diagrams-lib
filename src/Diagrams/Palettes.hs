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
       ( -- * Choosing color schemes

         Brightness(..), Kolor

         -- ** Color utilities

         , tint, tone, shade, rotateColor

         -- ** Color harmonies

         , monochrome
         , complement
         , triad
         , tetrad
         , analogic
         , accentAnalogic

         -- ** Colors from d3

         , colorSingles, colorPairs, colorQuads

         -- ** Painter's color

         , rybColors, rybColorA

         -- ** Common html colors

         , webColor, infiniteWebColors, getWebColor

       ) where

import Data.Array.IArray
import Data.List                (sortBy)
import Data.Colour
import Data.Colour.SRGB         (RGB(..), sRGB24read, toSRGB, sRGB)
import Data.Colour.RGBSpace.HSV
import Data.Colour.Names

type Kolor = Colour Double

-- | Tints a color by adding blending t * white + (1-t) color.
--   t should be between 0 and 1.
tint :: Double -> Kolor -> Kolor
tint t c = blend t white c

-- | Alter the tone of a color by adding blending t * gray + (1-t) color.
--   t should be between 0 and 1.
tone :: Double -> Kolor -> Kolor
tone t c = blend t gray c

-- | Shades a color by adding blending s * black + (1-s) color.
--   t should be between 0 and 1.
shade :: Double -> Kolor -> Kolor
shade s c = blend s black c

monochrome :: Kolor -> [Kolor]
monochrome c = [c, tint 0.4 c, tint 0.2 c, shade 0.25 c, shade 0.5 c]

mkChord :: Kolor -> [Double] -> [Kolor]
mkChord c xs = map (flip rotateColor c) xs

complement :: Kolor -> [Kolor]
complement c = [c, rotateColor 180 c, tint 0.4 c, tint 0.2 c, shade 0.25 c, shade 0.5 c]

triad :: Kolor -> [Kolor]
triad c = [c, rotateColor 240 c, rotateColor 120 c, darkgray, shade 0.6 c]

tetrad :: Kolor -> [Kolor]
tetrad c = mkChord c [0, 90, 180, 270] ++ [shade 0.6 c]

analogic :: Kolor -> [Kolor]
analogic c = mkChord c [0, 30, 15, 345, 330]

accentAnalogic :: Kolor -> [Kolor]
accentAnalogic c = mkChord c [0, 180, 30, 15, 345, 330]

numColors :: Int
numColors = length webColors

-- Number of colors to skip beteen choices from hueColors.
-- The idea is to skip enough similar colors so that the next color
-- is not too similar.
primeRepeat :: Int
primeRepeat = 61

-- This function and it's inverse below are the key to using the artists'
-- pigment color wheel. Red, blue and yellow are the primary colors and the
-- corresponding secondary colors are green, oragne and violet. We convert a
-- hue from the HSV in degrees (red = 0) to a hue on the artists' color wheel.
hsvToRyb :: Double -> Double
hsvToRyb x
  | x < 35    =       1.71 *  x
  | x < 60    = 60  + 2.40 * (x - 35)
  | x < 135   = 120 + 0.80 * (x - 60)
  | x < 225   = 180 + 0.67 * (x - 135)
  | x < 275   = 240 + 1.20 * (x - 225)
  | otherwise = 300 + 0.71 * (x - 275)

-- Convert of hue on the artists color wheel to a hue in HSV space.
rybToHsv :: Double -> Double
rybToHsv x
  | x < 60    =       0.58 *  x
  | x < 120   = 35  + 0.42 * (x - 60)
  | x < 180   = 60  + 1.25 * (x - 120)
  | x < 240   = 135 + 1.50 * (x - 180)
  | x < 300   = 225 + 0.83 * (x - 240)
  | otherwise = 275 + 1.42 * (x - 300)

rotateColor :: Double -> Kolor -> Kolor
rotateColor degrees c  = sRGB r g b
  where
    (h, s, v) = hsvView (toSRGB c)
    RGB r g b = hsv y s v
    k = (round $ hsvToRyb h + degrees :: Int) `mod` 360
    y = rybToHsv (fromIntegral k)



-- A few hundred common html colors -------------------------------------------
-------------------------------------------------------------------------------

-- | Choose the `n`th color in an array @a@ skipping @skip@ colors
--   every time.
getWebColor :: Array Int (Kolor) -> Int -> Int -> Kolor
getWebColor a skip n  = a ! idx
  where
    (i, k) = bounds a
    j = (n * skip) `mod` k
    idx = max i j

-- | Return a color from webColors arranged as to provide nice contrast
--   between near by colors.
webColor :: Int -> Kolor
webColor i = getWebColor webColorA primeRepeat (i+1) -- Start with a blue.

-- | A List of webColors ordered as above, cycling infinitely many times.
infiniteWebColors :: [Kolor]
infiniteWebColors = cycle [webColor j | j <- [0..numColors-1]]

-- Colors from d3. ------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Four levels of brightness for functions that take a @Brightness@ parameter.
--   For functions with only two levels of @Brightness@ we set @darkest == dark@
--   and @lightest == light@.
data Brightness = Darkest | Dark | Light | Lightest
  deriving (Eq)

-- | Choose from one of 10 contrasting colors (0-9) borrowed from mbostock's d3.
--
-- <<diagrams/src_Diagrams_Palettes_singles.svg#diagram=singles&width=400>>
--
-- > singles      = hcat (colorBar (const colorSingles) 0)
-- > colorBar m k = [square 1 # fc (m k i) | i <- [0..9]]
colorSingles :: Int ->  Kolor
colorSingles n = d3c10 ! (n `mod` 10)



-- | Choose 0 for dark and 1 for light for each pair of 10 sets of contrasting
--   colors (0-9) borrowed from mbostock's d3.
--
-- <<diagrams/src_Diagrams_Palettes_pairs.svg#diagram=pairs&width=400>>
--
-- > pairs      = vcat $ map (hcat . colorBar colorPairs) [Dark, Light]
colorPairs :: Brightness -> Int -> Kolor
colorPairs b n = d3c20 ! ((n `mod` 10), k)
  where k = if b == Darkest || b == Dark then 0 else 1



-- | Choose from 4 levels of darkness - 0 for darkest, 3 - for lightest. From
--   10 quadruples of contrasting colors (0-9) borrowed from mbostock's d3.
--
-- <<diagrams/src_Diagrams_Palettes_quads.svg#diagram=quads&width=400>>
--
-- > quads      = vcat $ map (hcat . colorBar colorQuads) [Darkest, Dark, Light, Lightest]
colorQuads :: Brightness -> Int -> Kolor
colorQuads b n =d3c20bc ! ((n `mod` 10), k)
  where k = case b of
              Darkest  -> 0
              Dark     -> 1
              Light    -> 2
              Lightest -> 3

-- Color data -----------------------------------------------------------------
-------------------------------------------------------------------------------

-- d3.scale.category10()
d3_10 :: [Kolor]
d3_10 = map sRGB24read
      [ "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"
      , "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"]

d3c10 :: Array Int (Kolor)
d3c10 = listArray (0,9) d3_10

-- d3.scale.category20()
d3_20 :: [Kolor]
d3_20 = map sRGB24read
      [ "#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#2ca02c"
      , "#98df8a", "#d62728", "#ff9896", "#9467bd", "#c5b0d5"
      , "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", "#7f7f7f"
      , "#c7c7c7", "#bcbd22", "#dbdb8d", "#17becf", "#9edae5"]

d3c20 ::  Array (Int, Int) (Kolor)
d3c20 = listArray ((0,0),(9,1)) d3_20

-- d3.scale.category20b() and d3.scale.category20c()
d3_40 :: [Kolor]
d3_40 = map sRGB24read
      [ "#393b79", "#5254a3", "#6b6ecf", "#9c9ede", "#637939"
      , "#8ca252", "#b5cf6b", "#cedb9c", "#8c6d31", "#bd9e39"
      , "#e7ba52", "#e7cb94", "#843c39", "#ad494a", "#d6616b"
      , "#e7969c", "#7b4173", "#a55194", "#ce6dbd", "#de9ed6"
      , "#3182bd", "#6baed6", "#9ecae1", "#c6dbef", "#e6550d"
      , "#fd8d3c", "#fdae6b", "#fdd0a2", "#31a354", "#74c476"
      , "#a1d99b", "#c7e9c0", "#756bb1", "#9e9ac8", "#bcbddc"
      , "#dadaeb", "#636363", "#969696", "#bdbdbd", "#d9d9d9"]

d3c20bc ::  Array (Int, Int) (Kolor)
d3c20bc = listArray ((0,0),(9,3)) d3_40

-- List of commonly used html colors.
htmlColors :: [Kolor]
htmlColors = map sRGB24read
  [ "#000000", "#2c3539", "#2b1b17", "#34282c", "#25383c", "#3b3131", "#413839"
  , "#463e3f", "#4c4646", "#504a4b", "#565051", "#5c5858", "#625d5d", "#666362"
  , "#6d6968", "#726e6d", "#736f6e", "#837e7c", "#848482", "#b6b6b4", "#d1d0ce"
  , "#e5e4e2", "#bcc6cc", "#98afc7", "#6d7b8d", "#657383", "#616d7e", "#646d7e"
  , "#566d7e", "#737ca1", "#4863a0", "#2b547e", "#2b3856", "#151b54", "#000080"
  , "#342d7e", "#15317e", "#151b8d", "#0000a0", "#0020c2", "#0041c2", "#2554c7"
  , "#1569c7", "#2b60de", "#1f45fc", "#6960ec", "#736aff", "#357ec7", "#488ac7"
  , "#3090c7", "#659ec7", "#87afc7", "#95b9c7", "#728fce", "#2b65ec", "#306eff"
  , "#157dec", "#1589ff", "#6495ed", "#6698ff", "#38acec", "#56a5ec", "#5cb3ff"
  , "#3bb9ff", "#79baec", "#82cafa", "#82caff", "#a0cfec", "#b7ceec", "#b4cfec"
  , "#c2dfff", "#c6deff", "#afdcec", "#addfff", "#bdedff", "#cfecec", "#e0ffff"
  , "#ebf4fa", "#f0f8ff", "#f0ffff", "#ccffff", "#93ffe8", "#9afeff", "#7fffd4"
  , "#00ffff", "#7dfdfe", "#57feff", "#8eebec", "#50ebec", "#4ee2ec", "#81d8d0"
  , "#92c7c7", "#77bfc7", "#78c7c7", "#48cccd", "#43c6db", "#46c7c7", "#43bfc7"
  , "#3ea99f", "#3b9c9c", "#438d80", "#348781", "#307d7e", "#5e7d7e", "#4c787e"
  , "#008080", "#4e8975", "#78866b", "#617c58", "#728c00", "#667c26", "#254117"
  , "#306754", "#347235", "#437c17", "#387c44", "#347c2c", "#347c17", "#348017"
  , "#4e9258", "#6aa121", "#4aa02c", "#41a317", "#3ea055", "#6cbb3c", "#6cc417"
  , "#4cc417", "#52d017", "#4cc552", "#54c571", "#99c68e", "#89c35c", "#85bb65"
  , "#8bb381", "#9cb071", "#b2c248", "#9dc209", "#a1c935", "#7fe817", "#59e817"
  , "#57e964", "#64e986", "#5efb6e", "#00ff00", "#5ffb17", "#87f717", "#8afb17"
  , "#6afb92", "#98ff98", "#b5eaaa", "#c3fdb8", "#ccfb5d", "#b1fb17", "#bce954"
  , "#edda74", "#ede275", "#ffe87c", "#ffff00", "#fff380", "#ffffc2", "#ffffcc"
  , "#fff8c6", "#fff8dc", "#f5f5dc", "#faebd7", "#ffebcd", "#f3e5ab", "#ece5b6"
  , "#ffe5b4", "#ffdb58", "#ffd801", "#fdd017", "#eac117", "#f2bb66", "#fbb917"
  , "#fbb117", "#ffa62f", "#e9ab17", "#e2a76f", "#deb887", "#ffcba4", "#c9be62"
  , "#e8a317", "#ee9a4d", "#c8b560", "#d4a017", "#c2b280", "#c7a317", "#c68e17"
  , "#b5a642", "#ada96e", "#c19a6b", "#cd7f32", "#c88141", "#c58917", "#af7817"
  , "#b87333", "#966f33", "#806517", "#827839", "#827b60", "#786d5f", "#493d26"
  , "#483c32", "#6f4e37", "#835c3b", "#7f5217", "#7f462c", "#c47451", "#c36241"
  , "#c35817", "#c85a17", "#cc6600", "#e56717", "#e66c2c", "#f87217", "#f87431"
  , "#e67451", "#ff8040", "#f88017", "#ff7f50", "#f88158", "#f9966b", "#e78a61"
  , "#e18b6b", "#e77471", "#f75d59", "#e55451", "#e55b3c", "#ff0000", "#ff2400"
  , "#f62217", "#f70d1a", "#f62817", "#e42217", "#e41b17", "#dc381f", "#c34a2c"
  , "#c24641", "#c04000", "#c11b17", "#9f000f", "#990012", "#8c001a", "#7e3517"
  , "#8a4117", "#7e3817", "#800517", "#810541", "#7d0541", "#7e354d", "#7d0552"
  , "#7f4e52", "#7f5a58", "#7f525d", "#b38481", "#c5908e", "#c48189", "#c48793"
  , "#e8adaa", "#edc9af", "#fdd7e4", "#fcdfff", "#ffdfdd", "#fbbbb9", "#faafbe"
  , "#faafba", "#f9a7b0", "#e7a1b0", "#e799a3", "#e38aae", "#f778a1", "#e56e94"
  , "#f660ab", "#fc6c85", "#f6358a", "#f52887", "#e45e9d", "#e4287c", "#f535aa"
  , "#ff00ff", "#e3319d", "#f433ff", "#d16587", "#c25a7c", "#ca226b", "#c12869"
  , "#c12267", "#c25283", "#c12283", "#b93b8f", "#7e587e", "#571b7e", "#583759"
  , "#4b0082", "#461b7e", "#4e387e", "#614051", "#5e5a80", "#6a287e", "#7d1b7e"
  , "#a74ac7", "#b048b5", "#6c2dc7", "#842dce", "#8d38c9", "#7a5dc7", "#7f38ec"
  , "#8e35ef", "#893bff", "#8467d7", "#a23bec", "#b041ff", "#c45aec", "#9172ec"
  , "#9e7bff", "#d462ff", "#e238ec", "#c38ec7", "#c8a2c8", "#e6a9ec", "#e0b0ff"
  , "#f9b7ff", "#d2b9d3", "#e9cfec", "#ebdde2", "#e3e4fa", "#fdeef4", "#fff5ee"
  , "#fefcff", "#ffffff" ]

webColors :: [Kolor]
webColors = sortBy (\x y -> f x `compare` f y) htmlColors
  where f = hue . toSRGB

webColorA :: Array Int (Kolor)
webColorA = listArray (0, numColors-1) webColors

rybColors :: [Kolor]
rybColors =  map sRGB24read
  [ "#ff0000", "#ff4900", "#ff7400", "#ff9200", "#ffaa00"
  , "#ffbf00", "#ffd300", "#ffe800", "#ffff00", "#ccf600"
  , "#9fee00", "#67e300", "#00cc00", "#00af64", "#009999"
  , "#0b61a4", "#1240ab", "#1b1bb3", "#3914af", "#530fad"
  , "#7109aa", "#a600a6", "#cd0074", "#e40045"]

rybColorA :: Array Int (Kolor)
rybColorA = listArray (0,23) rybColors