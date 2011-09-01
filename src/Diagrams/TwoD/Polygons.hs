{-# LANGUAGE TypeFamilies
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Polygon
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Different 2D convex polygons and stars.
--
-- * Information about polygons: <http://en.wikipedia.org/wiki/Polygon>
-----------------------------------------------------------------------------

module Diagrams.TwoD.Polygons(
        -- * Poligon's construction
        polyPolar, polySides, polyRegular
        -- * Poligon's centers
        -- $centers
        , centroid
        -- * Stars
        -- | Star is a polygon which self-intersects in a regular way.
        --   It could consist of one or more stars (like David's Star)
        , star, starSkip
        -- * Orientation of polygons
        , orientX, orientY
        -- * Typed interface
        , PolyType(..), StarOpts(..), PolygonOrientation(..), PolygonOpts(..), polyPoints
    ) where

import Data.Ord          (comparing)
import Data.List
import Control.Arrow     ((&&&))
import Control.Monad     (guard)

import Data.AffineSpace  ((.-.), (.+^))
import Data.VectorSpace  (sumV, magnitude, (^/), (<.>), (^*))
import Data.Default

import Diagrams.TwoD.Types
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Util (unitY)
import Graphics.Rendering.Diagrams
import Diagrams.Path

-- | Method used to determine the vertices of a polygon.
data PolyType a = PolyPolar  [a] [Double]
                  -- ^ A \"polar\" polygon.
                  --
                  --   * The first argument is a list of /central/
                  --   /angles/ from each vertex to the next.
                  --
                  --   * The second argument is a list of /radii/ from the
                  --   origin to each successive vertex.
                  --
                  --   To construct an /n/-gon, use a list of /n-1/
                  --   angles and /n/ radii.  Extra angles or radii
                  --   are ignored.
                  --
                  --   Cyclic polygons (with all vertices lying on a
                  --   circle) can be constructed using a second
                  --   argument of @(repeat r)@.

                | PolySides  [a] [Double]
                  -- ^ A polygon determined by the distance between
                  --   successive vertices and the angles formed by
                  --   three successive vertices.  The polygon will be
                  --   centered at the /centroid/ of its vertices.
                  --
                  --   * The first argument is a list of /vertex/
                  --   /angles/, giving the angle at each vertex from
                  --   the previous vertex to the next.  The first
                  --   angle in the list will be the angle ???
                  --
                  --   * The second argument is a list of side lengths.
                  --
                  --   To construct an /n/-gon, use a list of /n-2/
                  --   angles and /n-1/ edge lengths.  Extra angles or
                  --   lengths are ignored.

                | PolyRegular Int Double
                  -- ^ A regular polygon with the given number of
                  --   sides (first argument) and the given radius
                  --   (second argument).

-- | Options for creating \"star\" polygons, where the edges connect
--   possibly non-adjacent vertices.
data StarOpts = StarFun (Int -> Int)
                -- ^ Specify the order in which the vertices should be
                --   connected by a function that maps each vertex
                --   index to the index of the vertex that should come
                --   next.  Indexing of vertices begins at 0.

                --   XXX is that true?

              | StarSkip Int
                -- ^ Specify a star polygon by a \"skip\".  A skip of
                --   1 indicates a normal polygon, where edges go
                --   between successive vertices.  A skip of 2 means
                --   that edges will connect every second vertex,
                --   skipping one in between.  Generally, a skip of
                --   /n/ means that edges will connect every /n/th
                --   vertex.

-- | Determine how a polygon should be oriented.
data PolygonOrientation = NoOrient  -- ^ No special orientation; the first
                                    --   vertex will be at (1,0).
                                    --   This is the default.
                        | OrientToX -- ^ Orient so the botommost edge
                                    --   is parallel to the x-axis.
                        | OrientToY -- ^ Orient so the leftmost edge
                                    --   is parallel to the y-axis.
  deriving (Eq, Ord, Show, Read)

-- | Options for specifying a polygon.
data PolygonOpts a = PolygonOpts
                     { polyType       :: PolyType a
                       -- ^ Specification for the polygon's vertices.

                     , polyStar       :: StarOpts
                       -- ^ Is this a star polygon?

                     , polyOrient     :: PolygonOrientation
                       -- ^ Should a rotation be applied to the
                       --   polygon in order to orient it in a
                       --   particular way?

                     , polyCenter     :: P2
                       -- ^ Should a translation be applied to the
                       --   polygon in order to place the center at a
                       --   particular location?
                     }

-- | The default polygon is a regular pentagon of radius 1, centered
--   at the origin, aligned to the x-axis.
instance Default (PolygonOpts a) where
    def = PolygonOpts (PolyRegular 5 1) (StarSkip 1) OrientToX origin

polyPoints :: Angle a => PolygonOpts a -> [[P2]]
polyPoints po = sg
    where
        ps = case polyType po of
            PolyPolar ans szs -> polyPolar ans szs
            PolySides ans szs -> polySides' ans szs
            PolyRegular n r   -> polyRegular n r
        ori = case polyOrient po of
            OrientToX         -> orientX ps
            OrientToY         -> orientY ps
            NoOrient          -> ps
        sg = case polyStar po of
            StarFun f         -> star f ori
            StarSkip n        -> starSkip n ori

polyPolar :: Angle a => [a] -> [Double] -> [P2]
polyPolar _ [] = []
polyPolar ans (l:ls) = snd $ unzip xs
    where
        xs = (l, translateX l origin) : zipWith3 (\a s (s',x) -> (s, scale (s/s') $ rotate a x)) ans ls xs

polySides' :: Angle a => [a] -> [Double] -> [P2]
polySides' ans ls = scanl (.+^) origin
                      $ zipWith rotate ans' (map (unitY ^*) ls)
  where
    ans' = scanl (+) 0 ans

-- | Polygon is centered around 'origin'.
polySides :: Angle a => [a] -> [Double] -> [P2]
polySides ans ls = let p0 = polySides' ans ls in translate (origin .-. centroid p0) p0

-- | Regular /n/-polygon
polyRegular :: Int -> Double -> [P2]
polyRegular n r = polyPolar (take (n-1) . repeat $ (tau::Rad) / fromIntegral n)
                            (repeat r)

-- XXX use 'ala' here? And elsewhere with (origin .+^) etc.?
-- | Center of /n/-polygon as sum of vertexes divided by /n/
centroid :: [P2] -> P2
centroid = (origin .+^) . uncurry (^/) . (sumV &&& (fromIntegral . length)) . map (.-. origin)

groups :: (Int->Int) -> Int -> [[Int]]
groups f n = unfoldr (\xs -> guard (not $ null xs) >> return (let zs = g (head xs) in (zs, xs \\ zs))) [0..n-1]
    where
        g x0 = snd $ last $ takeWhile (\(_,ys) -> ys == nub ys) $ iterate (\(k,ys) -> (f k `mod` n, k:ys)) (x0, [])

-- | Generate star from the list of points by function in Zn-ring.
star :: (Int->Int) -> [P2] -> [[P2]]
star f xs = map (map (xs!!)) $ groups f (length xs)

-- | Generate regular star by taking every /k/ point in cyclic list
starSkip :: Int -> [P2] -> [[P2]]
starSkip k = star (+k)

orient :: (Ord a) => Bool -> (P2 -> a) -> (Double -> P2 -> P2) -> [P2] -> [P2]
orient _ _ _ [] = []
orient isMax fc ft xs = rotate a xs
    where
        mm :: (a -> a -> Ordering) -> [a] -> a
        mm        = if isMax then maximumBy else minimumBy
        (x1,x,x2) = mm (comparing (fc . sndOf3))
                       (zip3 (tail xs ++ take 1 xs) xs (last xs : init xs))
        x'        = mm (comparing fc) [x1, x2]
        v         = x' .-. x
        ex        = ft 1 origin .-. origin
        a         = Rad $ acos ((v <.> ex) / magnitude v)

        sndOf3 (_,b,_) = b

-- | takes lowermost vertex then take its lowermost neighbor and make this side horisontally
orientX :: [P2] -> [P2]
orientX = orient True getY translateX
  where getY (P (_,y)) = y

-- | takes leftmost vertex then take its leftmost neighbor and make this side vertically
orientY :: [P2] -> [P2]
orientY = orient False getX translateY
  where getX (P (x,_)) = x
