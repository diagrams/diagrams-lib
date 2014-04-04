1.1.0.4 (4 April 2014)
----------------------

  - Require `intervals-0.7`
  - Give `connectOutside` a reasonable behavior even when one or both
    endpoints has no trace

1.1.0.3 (19 March 2014)
----------------------

  - Allow `lens-4.1`

1.1.0.2 (18 March 2014)
-----------------------

  - Allow `optparse-applicative-0.8`

1.1.0.1 (9 March 2014)
----------------------

  - Allow `vector-space-points-0.2`

1.1 (8 March 2014)
------------------

* **New features**

    - Support for `Deformation`s, arbitrary (non-affine)
      transformations on objects such as points, paths, and located
      trails (though not on diagrams).

    - New functions `clipTo`, which clips a diagram's envelope and
      trace along with its visual representation, and `clipped`, which
      clips the diagram's visual representation but replaces its
      envelope and trace with those of the clipping path.

    - New `arrowV` function, for creating an arrow with the direction
      and magnitude of a given vector.

    - `gap` traversal, for setting the head and tail gaps of an arrow
      simultaneously.

    - Generalized types for `centerXY` and `snugXY`, based on new
      `basis` function from `diagrams-core

    - New 3D `Transform`s, alignment, and 3D-specific `Prelude`.

    - New `frame` function similar to `pad`, but increases the envelope
      of a diagram by an amount specified in local units in every direction
      irrespective of the local origin.

    - New `splitFills` function for pushing fill attributes down to
      subtrees containing only loops (mostly of relevance only to
      backend implementors).

* **New instances**

    - `Typeable` instances for all data types that are used as diagram
      primitives.
    - `Sectionable` instance for `FixedSegment`.

* **API changes**

    - `Angle` is now a type, rather than a class.  It uses a single
      internal representation for angles, and lenses `turn`, `rad,`
      and `deg` are supplied for constructing (using `@@`) and viewing
      (using `^.`) `Angle`s in various units.  In addition, the `Num`
      instance for `Angle` has been removed, eliminating a class of
      errors where a bare number is interpreted in units other than
      what you expect.

    - Removed `Num` instance for angles.

* **Dependency/version changes**

    - Require `lens >= 4.0`.
	- Allow `array-0.5`.
	- Allow `hashable-1.1`.
	- Remove `NumInstances` dependency.

* **Bug fixes**

    - Exclude joins in offsets on close segments (#160).
    - Exclude extra segment when joining loops in offset (#155).

* **Performance improvements**

    - `colorToSRGBA` function now avoids expensive matrix operations,
      offering dramatic speedups in rendering diagrams with many color
      attributes.

1.0.1 (26 January 2014)
-----------------------

    - Depend on `hashable` package, and add `Hashable` instance for
      `SizeSpec2D`.

      Technically, the PVP specifies that adding a new instance
      requires a major version bump.  However, I highly doubt anyone
      was making their own orphan `Hashable` instances before.  Feel
      free to yell at Brent if this breaks your build.

1.0.0.1 (28 November 2013)
--------------------------

    - Allow semigroups-0.12

1.0 (25 November 2013)
----------------------

* **New features**

    - New modules `Diagrams.TwoD.Arrow` and `Diagrams.TwoD.Arrowheads`
      for creating arrows.
    - New module `Diagrams.Backend.CmdLine`, providing a flexible
      framework for creating command-line-driven diagram rendering executables.
    - New functions in `Diagrams.Offset`: `offsetTrail` and
      `offsetPath` for one-sided offsets of trails and paths;
      `expandTrail` and `expandPath` for "stroking" trails and paths,
      computing a path whose fill corresponds to the stroke of the
      given trail or path.
    - New module `Diagrams.Tangent` for computing tangent and normal
      vectors of segments, trails, and paths.
    - New functions in `Diagrams.Align` to allow diagrams to be aligned by `Trace`
      called `snug`, `snugBy` and `snugCenter`
      and the ability to define other boundary functions for alignment. Functions
      `snugL`, `snugR`, etc. are included in `TwoD.Align`.
    - Lenses from `Control.Lens` are now used consistently for record fields
      throughout the library.
    - New function `angleRatio` for calculating the ratio between two angles.
    - Restricted identity functions `asTurn`, `asRad`, and `asDeg` for
      resolving type ambiguity
    - New miter limit attribute.
    - New function `annularWedge` in `TwoD.Arc`
    - New `avgScale` utility in `TwoD.Transform`, for backends which
      cannot fully implement freezing of line width
    - New function `heptagon`, a vast improvement over the linguistic
      frankenstein `septagon`.
    - New function `lookupName` (re-exported from `diagrams-core`) for
      simple lookups of named subdiagrams
    - New function `angleBetween` to calculate the angle between two
      vectors.
    - New function `arcBetween` to draw an arc between two given
      points.
    - A bunch of new modules containing types, primitives and
      utilities for constructing 3D diagrams: `Diagrams.ThreeD.Align`,
      `.Camera`, `.Light`, `.Shapes`, `.Transform`, `.Types`, and
      `.Vector`.  This is still a "feature preview" (in particular,
      appropriate 3D backends are still under construction).

* **New instances**

    - `AdditiveGroup` and `VectorSpace` instances for `Turn`, `Rad`, `Deg`
    - `Alignable` instance for `(->) e`
	- `Default` instances for `FillRule`, `FillRuleA`, `LineJoin`,
      `LineCap`, `FillColor`
	- `Show` instances for `FillRule`, `FillRuleA`

* **API changes**

    - `e` no longer exported from `Diagrams.Prelude`.
    - `Diagrams.BoundingBox` is no longer exported from `Diagrams.Prelude`.
    - Re-export `Diagrams.Core.pointDiagram` from `Diagrams.Prelude`.
    - Added `fromAlphaColour` method to `Color` class.
    - `&` renamed to `^&`
    - Stop re-exporting `tan`, `over`, and `both` from `Data.Colour`.
	- New coordinate lenses `_x`, `_y`, and `_z` for `R2`, `P2`, `R3`, `P3`
    - Export `fullTurn` from `Diagrams.Prelude`.
    - `Codomain (Located a)` is now `Point (Codomain a)` instead of
      `Located (Codomain a)`.
	- Export `domainBounds` from `Diagrams.Parametric`.
	- Adjusting functionality moved from `Diagrams.Parametric` to its
      own module, `Diagrams.Parametric.Adjust`.
    - Rename `strokeT` (and primed variant) to `strokeTrail`; rename
      `strokeLocT` to `strokeLocTrail`.
    - `ScaleInv` is now in its own module, `Diagrams.TwoD.Transform.ScaleInv`.
	- Re-export `Image` type (but not constructor) from `Diagrams.TwoD`
    - Removed `Floating` and `RealFloat` instances for `Turn` and `Deg`
    - `offsetSegment` now returns a `Located` instead of a tuple.
    - Removed `Num` and `Fractional` instances for `R2`.

* **Dependency/version changes**

    - Remove `newtype` dependency
    - New dependencies on `lens`, `tagged`, `optparse-applicative`,
      `filepath`, `safe`, `vector-space-points`, `MemoTrie`
    - Depend on `intervals >= 0.3 && < 0.5`.

* **Bug fixes**

    - Depend on `intervals 0.3`, which allows diagrams to build on
      Windows, by evading a GHCi linker bug which affects the FFI use in
      previous versions of intervals ([diagrams-contrib#14](https://github.com/diagrams/diagrams-contrib/issues/14))

    - Use point envelope at the origin for text objects instead of an
      empty envelope
      ([#115](https://github.com/diagrams/diagrams-lib/issues/115),
      [#116](https://github.com/diagrams/diagrams-lib/issues/116)).
    - Adjusting the end of a trail now works correctly ([#95](https://github.com/diagrams/diagrams-lib/issues/95)).
    - Only look for miter join on corners in `Diagrams.TwoD.Offset` ([#118](https://github.com/diagrams/diagrams-lib/issues/118)).
    - `wedge` from `Diagrams.TwoD.Arc` is now a Loop ([#99](https://github.com/diagrams/diagrams-lib/issues/99))

* **Performance improvements**

    - `R2` is now strict and `UNPACK`ed
    - Add strictness to `Offset`, `Segment`, `OffsetEnvelope`, and `SizeSpec2D`.
	- Make `getEnvelope` calculation for `Segment` more efficient by
      floating divisions out of the inner calculation.
    - Use a specialized `HasTrie` instance for `R2`.

0.7.1.1 (27 September 2013)
---------------------------

* allow semigroups-0.11

0.7.1 (11 September 2013)
-------------------------

* **New features**

    - New standard miter limit attribute
    - New functions `lineColorA`, `lineWidthA`, `lineMiterLimitA`,
      `fontSizeA` for directly applying attribute values
    - `setDefault2DAttributes` now sets default line cap (butt), line
      join (miter), and miter limit (10) attributes

* **New instances**

    - `Data.Default` instances for
        - `LineCap`
	- `LineJoin`
	- `LineMiterLimit`
	- `LineWidth`
	- `LineColor`
	- `FontSize`

0.7 (9 August 2013)
-------------------

* **New features**

    - New module `Diagrams.TwoD.Curvature`, for computing the
      curvature of 2D segments at any given point.
    - New module `Diagrams.Offset`, containing an `offsetSegment`
      function that builds a trail a fixed distance from the original
      segment.  This is a precursor to planned functions `offsetTrail`
      and `offsetPath`.
    - New function `Diagrams.TwoD.Transform.onBasis`, for extracting a
      matrix representation of a 2D transformation
    - New functions `extrudeEnvelope` and `intrudeEnvelope`, for
      extending or shrinking an envelope only in a certain direction.
    - Generalize the `Color` class to absolute colors.
      This addresses concerns raised in issue #66 by letting the backend
      choose which color space to render `Color` instances to. Functions are
      provided for backwards compatibility with the old semantics.
    - New function `scaleInvPrim` for creating a diagram from a single
      scale-invariant primitive.
    - New module `Diagrams.Parametric`, containing a collection of
      classes abstracting over "parametric" things: `Parametric`,
      `DomainBounds`, `EndValues`, `Sectionable`, and `HasArcLength`,
      with instances for segments, trails, and related things.
    - A big refactoring of segments and trails:
        - Segments can now be either "closed" or "open".
        - There are now two types of trails: "lines" (which travel
          from point A to point B) or "loops" (closed curves which end
          where they started). `Trail` is now a wrapper type which can
          contain both loops and lines.
        - There is a new `Located` wrapper type for adding locations to
          translation-invariant things.  `Path`s now consist of a
          collection of `Located Trail`s.
        - The `PathLike` class is now renamed to `TrailLike`; the
          `trailLike` function takes a `Located Trail` as input.
    - New convenience functions `boundaryFrom` and `boundaryFromMay`,
      for computing boundaries of subdiagrams.
    - Re-export from `diagrams-lib` a lot of things defined in
      `diagrams-core`, to make them easier for users to find.  Several
      new modules have been created as a result: `Diagrams.Query`,
      `Diagrams.Envelope`, `Diagrams.Trace`, and `Diagrams.Names`.
    - Export the `centroid` function from `Diagrams.Prelude`.
    - `triangle` is now a synonym for `eqTriangle`.

* **New instances**

    - `IsPrim` instances for `Path`, `Ellipsoid`, `Image`, `Text`, and
      `ScaleInv`
    - `Eq`, `Ord`, and `Show` instances for `SizeSpec2D`

* **API changes**

    - `CircleFrac` has been renamed `Turn` (though `CircleFrac` is
      retained as a deprecated synonym).
    - `Diagrams.Coordinates` is no longer exported from
      `Diagrams.Prelude`.  This is for compatibility with `lens`, as `(&)`
      is a rather important lens operator and clashes with
      `Diagrams.Coordinates`.  Users who want the `Coordinates` stuff can import
      `Diagrams.Coordinates` explicitly.

* **Dependency/version changes**

    - allow `base-4.7`
    - upgrade to `monoid-extras-0.3`
    - depend on `data-default-class` instead of `data-default`
    - Tested with GHC 7.7.

* **Bug fixes**

    - Added a special case that was a not handled properly by the
      quadratic solver, resulting in bogus envelopes in certain cases
      (#88).
    - Import only `Data.NumInstances.Tuple` instead of
      `Data.NumInstances`. Previously, `Diagrams.Prelude` exported
      `Eq`, `Show`, and `Num` instances for functions and tuples; now
      it only exports tuple instances. Users wishing to use
      `Diagrams.CubicSpline` with a vector space built over functions (!?)
      can import `Data.NumInstances.Function` themselves. (#48)
    - Do scaling on a `Path` *before* constructing a `TrailLike` in
      `rect` (#43)

0.6.0.3 (4 May 2013)
--------------------

* bump upper bound to allow `NumInstances-1.3`

0.6.0.2 (28 March 2013)
-----------------------

* bump upper bound to allow `NumInstances-1.2`

* Quadratic solver is now more numerically stable, getting rid of some
  incorrect behavior of `juxtapose`
  ([\#46](https://github.com/diagrams/diagrams-lib/issues/46))

0.6.0.1: 7 January 2013
-----------------------

* allow `semigroups-0.9`

0.6: 11 December 2012
---------------------

* **New features**

    - `boundingRect` function for constructing a bounding rectangle

    - `bg` function for "setting the background color" (*i.e.* placing
      atop a colored bounding rectangle)

    - New functions `setDefault2DAttributes` and `adjustDiaSize2D`.
      `adjustDia2D` does both --- so the behavior of `adjustDia2D` has
      not changed, but it is now possible to apply just one of the two
      adjustments using the new functions.

    - `Diagrams.TwoD.Transform` now exports a `ScaleInv` type for creating
      scale-invariant objects, which are only affected by rotational
      and translational components of transformations.

    - The new `Diagrams.Coordinates` module provides nicer syntax for
      constructing and pattern-matching point and vector literals.

    - New `fromFixedSeg` function in `Diagrams.Segment`, which
      decomposes a `FixedSegment` into a starting point and a `Segment`.

    - New `withTrace` function for setting the `Trace` of a diagram.

    - Three new size-related functions:

        - New `sized` function for scaling an object to a particular size.
          One particularly nice use of this is to obviate the need to keep
          fiddling with the line width to get diagrams to "look right";
          just set the line width relative to some arbitrary scale
          (*e.g.* assuming the final diagram will fit into a 1x1 box) and
          then apply `sized` to the final diagram to make it that given
          arbitrary size.  It can also be used for easily making something
          (a diagram, path, trail, ...) the same size as something else,
          with the help of the new `sizeSpec2D` function.

        - New `sizedAs` function, for setting the size of some object to
          be "the same as" some other object.

        - New `sizeSpec2D` function for conveniently calculating the size
          of an object as a `SizeSpec2D` value (for use with the new `sized`
          funtion).

    - New `extrudeEnvelope` and `intrudeEnvelope` functions for
      modifying envelopes in a single direction only, as well as new
      functions `extrude{Left,Right,Bottom,Top}` specializing
      `extrudeEnvelope` to 2D.

    - `arcCW` draws clockwise arcs; `arc'` draws arcs counterclockwise
      or clockwise as the radius is positive or negative,
      respectively.

    - fill color attribute is generalized to support "recommended" and
      "committed" colors; text objects use a recommended fill color of
      black.

* **New instances**

    - The `Show` instance for `R2` now produces something like `2 & 6`
      instead of `R2 { unR2 = (2,6) }`.  The `Read` instance has also
      been changed to match, so `read . show = id`.

    - `Enveloped` instance for `FixedSegment`

    - `Traced` instances for `Segment`, `FixedSegment`, `Trail`, and `Path`

    - New derived `Eq` instances for `LineCapA`, `LineJoinA`, `Dashing`,
      `DashingA`, `FillRule`, `Font`, `FontSize`, `FontSlant`, `FontSlantA`,
      `FontWeight`, and `FontWeightA`

    - `Renderable Ellipsoid NullBackend` instance

* **API changes**

    - `Data.Colour` (minus `atop` and `AffineSpace`) is now re-exported
      from Diagrams.Prelude for convenience.

    - The `beneath` function is now infixl 6.

    - The `BoundingBox` module has had a complete overhaul.  There is
      now a special empty bounding box, and bounding boxes are an
      instance of `Monoid`.

    - The type of `withEnvelope` has been slightly generalized.

    - `Diagrams.TwoD.Adjust.adjustSize` is now deprecated; it has been
      renamed and moved to `Diagrams.TwoD.Size.requiredScaleT`.

    - `expandPath` has been renamed to `scalePath`.

* **Dependency/version changes**

    - Allow `data-default` 0.4 and 0.5
    - Allow `base`-4.6
    - Allow `containers`-0.5

* **Bug fixes**

    - `arc` and `arcT` functions now always produce counterclockwise arcs,
      as claimed.

0.5: 9 March 2012
-----------------

* **New features**
    - `mkSizeSpec` function for constructing a `SizeSpec2D` from two `Maybe Double`s
    - `beneath` as convenient synonym for `flip atop`
    - Improvements and extensions to rounded rectangles by Peter Hall:
        + `roundedRect'` allows rounded rectangles with a different radius
          specified for each corner
        + both `roundedRect'` and `roundedRect` now allow negative radii,
          resulting in "inverted" circular corners
    - [\#64](http://code.google.com/p/diagrams/issues/detail?id=64): New `Alignable` class for things that can be aligned.
    - `explodeTrail` and `explodePath` have been generalized to return any
      `PathLike` type.
    - New path functions `pathCentroid` (compute the centroid of a
      path's vertices) and `expandPath` (scale a path about its centroid).
    - Generalized `centroid` function now exported from new module
      `Diagrams.Points`.
    - Initial (experimental) support for animation:
        + `Animation` and `QAnimation` defined as synonyms for `Active`
          diagrams (see `active` package)
        + Instances for `Active`: `V`, `HasOrigin`, `Transformable`,
          `HasStyle`, `PathLike`, `Juxtaposable`, `Alignable`
        + `animEnvelope` and `animRect` functions for automatic bounding of
          animations
    - `addClosingSegment` function for making the implicit closing
      segment of a closed trail explicit
    - Improvements to `BoundingBox` module from Michael Sloan: querying
      of `BoundingBox` bounds, corners, extents, and transformation of
      objects to fit within a given box.
    - Text alignment options from Michael Sloan
    - `view` function for restricting a diagram's envelope to a
      rectangular region.
    - `iterateN` function for iterating a finite number of times
    - `atAngle` for placing two diagrams next to each other along a
      specified angle.
    - `padX` and `padY` functions for padding in the X- and Y-directions
      independently.
    - generalized `showOrigin` function from Ian Ross
    - [\#40](http://code.google.com/p/diagrams/issues/detail?id=40): add shears to `Diagrams.TwoD.Transform`

* **Performance improvements**
    - Use a balanced folding scheme for `cat'`, reducing time in some
      cases from \\(O(n^2)\\) to \\(O(n \\log n)\\)
    - More efficient implementation of `beside`

* **New instances**
    - `Alignable` instances for `QDiagram`, `Path`, `Envelope`, `Active`, `Set`,
      `Map`, `[]`
    - `Renderable` instances for `NullBackend` (`Trail`, `Path`, `Segment`,
      `Image`, `Text`)
    - Instances for `Active`: `V`, `HasOrigin`, `Transformable`,
      `HasStyle`, `PathLike`, `Juxtaposable`, `Alignable`

* **API changes**
    - `R2` used to be a synonym for `(Double, Double)` but is now
      abstract. To convert between pairs of `Doubles` and `R2`, use the
      new functions `r2` and `unr2`.  There are two reasons for this
      change:
          1. to allow for future changes to the implementation of `R2`;
          2. `(Double, Double)` was an awkward special case getting in the
           way of useful tuple instances for classes like `HasOrigin`,
           `Enveloped`, and so on.
    - `circlePath` has been removed; its functionality has been
      subsumed by `circle`.
    - `adjustSegment` now takes an extra tolerance option.
    - Ellipses are now represented using Bezier approximations rather
      than a separate special type.
    - `BoundingBox` no longer has a `Transformable` instance; the old
      instance was misleading at best.
    - Change semantics of `beside` (hence also `(|||)` and `(===)`) so the
      result's origin is the same as that of the first argument.
    - `adjustDia2D` now takes a `SizeSpec2D`.
    - `beside` and related functions are now implemented in terms of
      `juxtapose`.
    - Instead of taking an `R2`, `roundedRect` now takes a pair of
      `Double`s, to be more consistent with `rect`.

* **Dependency/version changes**
    - Support for GHC 7.4.1:
        + depend on `colour` >= 2.3.2
        + update `base` and `array` upper bounds
    - bump `vector-space` upper bound

* **Bug fixes**
    - Avoid scale by zero error in `showOrigin`.
    - Base `adjustDia2D` translation on output size rather than diagram size.

0.4.0.1: 30 October 2011
------------------------

* bump `data-default` dependency to allow version 0.3

0.4: 23 October 2011
--------------------

* **documentation fixes**

* **New functions and primitives**

    + `wedge` shape primitive
    + `fromDirection` function for converting angles to 2D unit vectors;
      inverse function `direction` generalized to return any Angle type
    + New functions for computing and adjusting segment lengths
    + `scaleUToX` and `scaleUToY` for doing uniform scales
      resulting in a desired width or height.
    + `circlePath`, `reversePath`, `decoratePath`

* **New features**

    + Completely new and improved polygon generation API
    + Cubic splines
    + User-controllable path fill rules

* **Bug fixes**

    + fix incorrect corner case in arc generation
    + fix incorrect `reverseTrail` function

0.3: 18 June 2011
-----------------

* **New features**
    + new customizable `stroke'` function which lets you assign names to
      path vertices
    + `circle` and `square` functions now take a size argument
    + function for adjusting 2D diagrams to requested size abstracted
      from cairo backend
    + generalize `PathLike` class to include an instance for diagrams,
      and collapse things like `polygon`/`polygonPath` into a single
      polymorphic function
    + basic text support
    + basic support for external images
    + very sketchy initial proof-of-concept library of 3D primitives.
      See also diagrams-povray package.

* **Bug fixes**
    + Issue 32 (`mempty` not behaving correctly within concatenations)

0.2: 3 June 2011
----------------

* **documentation fixes**

* **New functions and primitives**
    + `scaleToX` and `scaleToY` for scaling to an absolute width/height
    + `reverseTrail`
    + new `Angle` class and ability to use radians, degrees, or circle fractions
      for specifying angles
    + `rotateAbout` and `reflectAbout` transformations based on new conjugation functions
    + `rect` and `roundedRect` primitives
    + `explodeTrail`/`Path` for breaking trails and paths into individual segments

* **New features**
    + opacity attribute
    + support for path clipping

* **New modules**
    + `Diagrams.BoundingBox`

* **Fixes and updates**
    + `withBounds` now properly uses the new bounds instead of just combining
      them with the old

0.1.1: 18 May 2011
------------------

* minor documentation fixes
* link to new website

0.1: 17 May 2011
----------------

* initial preview release
