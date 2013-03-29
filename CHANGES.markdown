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

