{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Attributes
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Diagrams may have /attributes/ which affect the way they are
-- rendered.  This module defines some common attributes; particular
-- backends may also define more backend-specific attributes.
--
-- Every attribute type must have a /semigroup/ structure, that is, an
-- associative binary operation for combining two attributes into one.
-- Unless otherwise noted, all the attributes defined here use the
-- 'Last' structure, that is, combining two attributes simply keeps
-- the second one and throws away the first.  This means that child
-- attributes always override parent attributes.
--
-----------------------------------------------------------------------------

module Diagrams.Attributes (
  -- * Color
  -- $color

    Color(..), SomeColor(..), someToAlpha

  -- ** Line color
  , LineColor, getLineColor, mkLineColor, styleLineColor, lineColor, lineColorA, lc, lcA

  -- ** Fill color
  , FillColor, getFillColor, mkFillColor, styleFillColor, recommendFillColor, fillColor, fc, fcA

  -- ** Opacity
  , Opacity, getOpacity, opacity

  -- ** Converting colors
  , colorToSRGBA, colorToRGBA

  -- * Lines
  -- ** Width
  , LineWidth, getLineWidth, lineWidth, lineWidthA, lw

  -- ** Cap style
  , LineCap(..), LineCapA, getLineCap, lineCap

  -- ** Join style
  , LineJoin(..), LineJoinA, getLineJoin, lineJoin

  -- ** Miter limit
  , LineMiterLimit(..), getLineMiterLimit, lineMiterLimit, lineMiterLimitA

  -- ** Dashing
  , Dashing(..), DashingA, getDashing, dashing

  -- * Compilation utilities
  , splitFills

  ) where

import           Control.Arrow         (second)
import           Control.Lens          (Setter, sets, (%~), (&), _Wrapping')
import           Data.Colour
import           Data.Colour.RGBSpace  (RGB (..))
import           Data.Colour.SRGB      (toSRGB)
import           Data.Default.Class
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           Data.Monoid.Recommend
import           Data.Semigroup
import           Data.Tree
import           Data.Typeable

import           Diagrams.Core
import           Diagrams.Core.Style   (Style (..), attrToStyle, setAttr)
import           Diagrams.Core.Types   (RNode (..), RTree)
import           Diagrams.Located      (unLoc)
import           Diagrams.Path         (Path, pathTrails)
import           Diagrams.Trail        (isLine)

------------------------------------------------------------
--  Color  -------------------------------------------------
------------------------------------------------------------

-- $color
-- Diagrams outsources all things color-related to Russell O\'Connor\'s
-- very nice colour package
-- (<http://hackage.haskell.org/package/colour>).  For starters, it
-- provides a large collection of standard color names.  However, it
-- also provides a rich set of combinators for combining and
-- manipulating colors; see its documentation for more information.

-- | The 'Color' type class encompasses color representations which
--   can be used by the Diagrams library.  Instances are provided for
--   both the 'Data.Colour.Colour' and 'Data.Colour.AlphaColour' types
--   from the "Data.Colour" library.
class Color c where
  -- | Convert a color to its standard representation, AlphaColour.
  toAlphaColour :: c -> AlphaColour Double

  -- | Convert from an AlphaColour Double.  Note that this direction
  --   may lose some information. For example, the instance for
  --   'Colour' drops the alpha channel.
  fromAlphaColour :: AlphaColour Double -> c

-- | An existential wrapper for instances of the 'Color' class.
data SomeColor = forall c. Color c => SomeColor c
  deriving Typeable

someToAlpha :: SomeColor -> AlphaColour Double
someToAlpha (SomeColor c) = toAlphaColour c

-- | The color with which lines (strokes) are drawn.  Note that child
--   colors always override parent colors; that is, @'lineColor' c1
--   . 'lineColor' c2 $ d@ is equivalent to @'lineColor' c2 $ d@.
--   More precisely, the semigroup structure on line color attributes
--   is that of 'Last'.
newtype LineColor = LineColor (Last SomeColor)
  deriving (Typeable, Semigroup)
instance AttributeClass LineColor

instance Default LineColor where
    def = LineColor (Last (SomeColor (black :: Colour Double)))

getLineColor :: LineColor -> SomeColor
getLineColor (LineColor (Last c)) = c

mkLineColor :: Color c => c -> LineColor
mkLineColor = LineColor . Last . SomeColor

styleLineColor :: (Color c, Color c') => Setter (Style v) (Style v) c c'
styleLineColor = sets modifyLineColor
  where
    modifyLineColor f s
      = flip setAttr s
      . mkLineColor
      . f
      . fromAlphaColour . someToAlpha
      . getLineColor
      . fromMaybe def . getAttr
      $ s

-- | Set the line (stroke) color.  This function is polymorphic in the
--   color type (so it can be used with either 'Colour' or
--   'AlphaColour'), but this can sometimes create problems for type
--   inference, so the 'lc' and 'lcA' variants are provided with more
--   concrete types.
lineColor :: (Color c, HasStyle a) => c -> a -> a
lineColor = applyAttr . mkLineColor

-- | Apply a 'lineColor' attribute.
lineColorA :: HasStyle a => LineColor -> a -> a
lineColorA = applyAttr

-- | A synonym for 'lineColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).
lc :: HasStyle a => Colour Double -> a -> a
lc = lineColor

-- | A synonym for 'lineColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).
lcA :: HasStyle a => AlphaColour Double -> a -> a
lcA = lineColor

-- | The color with which shapes are filled. Note that child
--   colors always override parent colors; that is, @'fillColor' c1
--   . 'fillColor' c2 $ d@ is equivalent to @'lineColor' c2 $ d@.
--   More precisely, the semigroup structure on fill color attributes
--   is that of 'Last'.
newtype FillColor = FillColor (Recommend (Last SomeColor))
  deriving (Typeable, Semigroup)
instance AttributeClass FillColor

instance Default FillColor where
  def = FillColor (Recommend (Last (SomeColor (transparent :: AlphaColour Double))))

mkFillColor :: Color c => c -> FillColor
mkFillColor = FillColor . Commit . Last . SomeColor

styleFillColor :: (Color c, Color c') => Setter (Style v) (Style v) c c'
styleFillColor = sets modifyFillColor
  where
    modifyFillColor f s
      = flip setAttr s
      . mkFillColor
      . f
      . fromAlphaColour . someToAlpha
      . getFillColor
      . fromMaybe def . getAttr
      $ s

-- | Set the fill color.  This function is polymorphic in the color
--   type (so it can be used with either 'Colour' or 'AlphaColour'),
--   but this can sometimes create problems for type inference, so the
--   'fc' and 'fcA' variants are provided with more concrete types.
fillColor :: (Color c, HasStyle a) => c -> a -> a
fillColor = applyAttr . mkFillColor

-- | Set a \"recommended\" fill color, to be used only if no explicit
--   calls to 'fillColor' (or 'fc', or 'fcA') are used.
recommendFillColor :: (Color c, HasStyle a) => c -> a -> a
recommendFillColor = applyAttr . FillColor . Recommend . Last . SomeColor

getFillColor :: FillColor -> SomeColor
getFillColor (FillColor c) = getLast . getRecommend $ c

-- | A synonym for 'fillColor', specialized to @'Colour' Double@
--   (i.e. opaque colors).
fc :: HasStyle a => Colour Double -> a -> a
fc = fillColor

-- | A synonym for 'fillColor', specialized to @'AlphaColour' Double@
--   (i.e. colors with transparency).
fcA :: HasStyle a => AlphaColour Double -> a -> a
fcA = fillColor

instance (Floating a, Real a) => Color (Colour a) where
  toAlphaColour   = opaque . colourConvert
  fromAlphaColour = colourConvert . (`over` black)

instance (Floating a, Real a) => Color (AlphaColour a) where
  toAlphaColour   = alphaColourConvert
  fromAlphaColour = alphaColourConvert

instance Color SomeColor where
  toAlphaColour (SomeColor c) = toAlphaColour c
  fromAlphaColour c = SomeColor c

instance Color LineColor where
  toAlphaColour (LineColor c) = toAlphaColour . getLast $ c
  fromAlphaColour = LineColor . Last . fromAlphaColour

instance Color FillColor where
  toAlphaColour (FillColor c) = toAlphaColour . getLast . getRecommend $ c
  fromAlphaColour = FillColor . Commit . Last . fromAlphaColour

-- | Convert to sRGBA.
colorToSRGBA, colorToRGBA :: Color c => c -> (Double, Double, Double, Double)
colorToSRGBA col = (r, g, b, a)
  where
    c' = toAlphaColour col
    c = alphaToColour c'
    a = alphaChannel c'
    RGB r g b = toSRGB c

colorToRGBA = colorToSRGBA
{-# DEPRECATED colorToRGBA "Renamed to colorToSRGBA." #-}

alphaToColour :: (Floating a, Ord a, Fractional a) => AlphaColour a -> Colour a
alphaToColour ac | alphaChannel ac == 0 = ac `over` black
                 | otherwise = darken (recip (alphaChannel ac)) (ac `over` black)

------------------------------------------------------------
-- Opacity

-- | Although the individual colors in a diagram can have
--   transparency, the opacity/transparency of a diagram as a whole
--   can be specified with the @Opacity@ attribute.  The opacity is a
--   value between 1 (completely opaque, the default) and 0
--   (completely transparent).  Opacity is multiplicative, that is,
--   @'opacity' o1 . 'opacity' o2 === 'opacity' (o1 * o2)@.  In other
--   words, for example, @opacity 0.8@ means \"decrease this diagram's
--   opacity to 80% of its previous opacity\".
newtype Opacity = Opacity (Product Double)
  deriving (Typeable, Semigroup)
instance AttributeClass Opacity

getOpacity :: Opacity -> Double
getOpacity (Opacity (Product d)) = d

-- | Multiply the opacity (see 'Opacity') by the given value.  For
--   example, @opacity 0.8@ means \"decrease this diagram's opacity to
--   80% of its previous opacity\".
opacity :: HasStyle a => Double -> a -> a
opacity = applyAttr . Opacity . Product

------------------------------------------------------------
--  Lines and stuff    -------------------------------------
------------------------------------------------------------

-- | The width of lines.  By default, the line width is measured with
--   respect to the /final/ coordinate system of a rendered diagram,
--   as opposed to the local coordinate systems in effect at the time
--   the line width was set for various subdiagrams.  This is so that
--   it is easy to combine a variety of shapes (some created by
--   scaling) and have them all drawn using a consistent line width.
--   However, sometimes it is desirable for scaling to affect line
--   width; the 'freeze' operation is provided for this purpose.  The
--   line width of frozen diagrams is affected by transformations.
--
--   Line widths specified on child nodes always override line widths
--   specified at parent nodes.
newtype LineWidth = LineWidth (Last Double)
  deriving (Typeable, Semigroup)
instance AttributeClass LineWidth

instance Default LineWidth where
    def = LineWidth (Last 0.01)

getLineWidth :: LineWidth -> Double
getLineWidth (LineWidth (Last w)) = w

-- | Set the line (stroke) width.
lineWidth :: HasStyle a => Double -> a -> a
lineWidth = applyAttr . LineWidth . Last

-- | Apply a 'LineWidth' attribute.
lineWidthA ::  HasStyle a => LineWidth -> a -> a
lineWidthA = applyAttr

-- | A convenient synonym for 'lineWidth'.
lw :: HasStyle a => Double -> a -> a
lw = lineWidth

-- | What sort of shape should be placed at the endpoints of lines?
data LineCap = LineCapButt   -- ^ Lines end precisely at their endpoints.
             | LineCapRound  -- ^ Lines are capped with semicircles
                             --   centered on endpoints.
             | LineCapSquare -- ^ Lines are capped with a squares
                             --   centered on endpoints.
  deriving (Eq,Show,Typeable)

newtype LineCapA = LineCapA (Last LineCap)
  deriving (Typeable, Semigroup, Eq)
instance AttributeClass LineCapA

instance Default LineCap where
    def = LineCapButt

getLineCap :: LineCapA -> LineCap
getLineCap (LineCapA (Last c)) = c

-- | Set the line end cap attribute.
lineCap :: HasStyle a => LineCap -> a -> a
lineCap = applyAttr . LineCapA . Last


-- | How should the join points between line segments be drawn?
data LineJoin = LineJoinMiter    -- ^ Use a \"miter\" shape (whatever that is).
              | LineJoinRound    -- ^ Use rounded join points.
              | LineJoinBevel    -- ^ Use a \"bevel\" shape (whatever
                                 --   that is).  Are these...
                                 --   carpentry terms?
  deriving (Eq,Show,Typeable)

newtype LineJoinA = LineJoinA (Last LineJoin)
  deriving (Typeable, Semigroup, Eq)
instance AttributeClass LineJoinA

instance Default LineJoin where
    def = LineJoinMiter

getLineJoin :: LineJoinA -> LineJoin
getLineJoin (LineJoinA (Last j)) = j

-- | Set the segment join style.
lineJoin :: HasStyle a => LineJoin -> a -> a
lineJoin = applyAttr . LineJoinA . Last


-- | Miter limit attribute affecting the 'LineJoinMiter' joins.
--   For some backends this value may have additional effects.
newtype LineMiterLimit = LineMiterLimit (Last Double)
  deriving (Typeable, Semigroup)
instance AttributeClass LineMiterLimit

instance Default LineMiterLimit where
    def = LineMiterLimit (Last 10)

getLineMiterLimit :: LineMiterLimit -> Double
getLineMiterLimit (LineMiterLimit (Last l)) = l

-- | Set the miter limit for joins with 'LineJoinMiter'.
lineMiterLimit :: HasStyle a => Double -> a -> a
lineMiterLimit = applyAttr . LineMiterLimit . Last

-- | Apply a 'LineMiterLimit' attribute.
lineMiterLimitA :: HasStyle a => LineMiterLimit -> a -> a
lineMiterLimitA = applyAttr

-- | Create lines that are dashing... er, dashed.
data Dashing = Dashing [Double] Double
  deriving (Typeable, Eq)

newtype DashingA = DashingA (Last Dashing)
  deriving (Typeable, Semigroup, Eq)
instance AttributeClass DashingA

getDashing :: DashingA -> Dashing
getDashing (DashingA (Last d)) = d

-- | Set the line dashing style.
dashing :: HasStyle a =>
           [Double]  -- ^ A list specifying alternate lengths of on
                     --   and off portions of the stroke.  The empty
                     --   list indicates no dashing.
        -> Double    -- ^ An offset into the dash pattern at which the
                     --   stroke should start.
        -> a -> a
dashing ds offs = applyAttr (DashingA (Last (Dashing ds offs)))

------------------------------------------------------------

-- | Push fill attributes down until they are at the roots of trees
--   containing only loops.
splitFills :: forall b v a. Typeable v => RTree b v a -> RTree b v a
splitFills = fst . splitFills' Nothing
  where

  -- splitFills' is where the most interesting logic happens.
  -- Mutually recursive with splitFills'Forest. rebuildNode and
  -- applyMfc are helper functions.
  --
  -- Input: fill attribute to apply to subtrees containing only loops.
  --
  -- Output: tree with fill attributes pushed down appropriately, and
  -- a Bool indicating whether the tree contains only loops (True) or
  -- contains some lines (False).
  splitFills' :: Maybe FillColor -> RTree b v a -> (RTree b v a, Bool)

  -- RStyle node: Check for fill attribute and split it out of the
  -- style, combining it with incoming fill attribute.  Recurse and
  -- rebuild.
  splitFills' mfc (Node (RStyle sty) cs) = (t', ok)
    where
      mfc' = mfc <> getAttr sty
      sty' = sty & _Wrapping' Style %~ M.delete ty
      ty   = show . typeOf $ (undefined :: FillColor)
      (cs', ok) = splitFills'Forest mfc' cs
      t' | ok        = rebuildNode Nothing ok (RStyle sty) cs'
         | otherwise = rebuildNode mfc ok (RStyle sty') cs'

  -- RPrim node: check whether it
  --   * is not a path : don't apply the fill; return True
  --   * contains lines: don't apply the fill; return False
  --   * contains loops:  do   apply the fill; return True
  splitFills' mfc (Node rp@(RPrim _ (Prim p)) _) =
      case cast p :: Maybe (Path v) of
        Nothing  -> (Node rp [], True)
        Just pth ->
          case any (isLine . unLoc) . pathTrails $ pth of
            True  -> (Node rp [], False)
            False -> (rebuildNode mfc True rp [], True)

  -- RFrozenTr, RAnnot, REmpty cases: just recurse and rebuild.  Note
  -- that transformations do not affect fill attributes.
  splitFills' mfc (Node nd cs)    = (t', ok)
    where
      (cs', ok) = splitFills'Forest mfc cs
      t'        = rebuildNode mfc ok nd cs'

  -- Recursively call splitFills' on all subtrees, returning the
  -- logical AND of the Bool results returned (the whole forest
  -- contains only loops iff all the subtrees do).  The tricky bit is
  -- that we use some knot-tying to determine the right attribute to pass
  -- down to the subtrees based on this computed Bool: if all subtrees
  -- contain only loops, then we will apply the fill at the root of
  -- this forest, and pass Nothing to all the subtrees.  Otherwise, we
  -- pass the given fill along.  This works out because the fill
  -- does not need to be pattern-matched until actually applying it at
  -- some root, so the recursion can proceed and the Bool values be
  -- computed with the actual value of the fill nodes filled in
  -- lazily.
  splitFills'Forest :: Maybe FillColor -> [RTree b v a] -> ([RTree b v a], Bool)
  splitFills'Forest mfc cs = (cs', ok)
    where
      (cs', ok) = second and . unzip . map (splitFills' mfc') $ cs
      mfc' | ok        = Nothing
           | otherwise = mfc

  -- Given a fill attribute, a Bool indicating whether the given
  -- subforest contains only loops, a node, and a subforest, rebuild a
  -- tree, applying the fill attribute as appropriate (only if the
  -- Bool is true and the attribute is not Nothing).
  rebuildNode :: Maybe FillColor -> Bool -> RNode b v a -> [RTree b v a] -> RTree b v a
  rebuildNode mfc ok nd cs
    | ok        = applyMfc mfc (Node nd cs)
    | otherwise = Node nd cs

  -- Prepend a new fill color node if Just; the identity function if
  -- Nothing.
  applyMfc :: Maybe FillColor -> RTree b v a -> RTree b v a
  applyMfc Nothing  t = t
  applyMfc (Just f) t = Node (RStyle $ attrToStyle f) [t]

