## [v1.4.5.3](https://github.com/diagrams/diagrams-lib/tree/v1.4.5.3) (2022-09-19)

- Update to `fsnotify-0.4`

- Revisions:
  - r1: require `lens-5.1`
  - r2: allow `mtl-2.3`
  - r3 (3 Dec 2022): allow `linear-1.22`

## [v1.4.5.2](https://github.com/diagrams/diagrams-lib/tree/v1.4.5.2) (2022-09-14)

- Fix build with `transformers-0.6`
- Fix some deprecation warnings

## [v1.4.5.1-r3](https://github.com/diagrams/diagrams-lib/tree/v1.4.5.1-r3) (2022-08-27)

- Allow `base-4.17` and `lens-5.2`; test with GHC 9.4

## [v1.4.5.1-r2](https://github.com/diagrams/diagrams-lib/tree/v1.4.5.1-r2) (2022-02-01)

- Allow `optparse-applicative-0.17`

## [v1.4.5.1-r1](https://github.com/diagrams/diagrams-lib/tree/v1.4.5.1-r1) (2022-01-08)

- Allow `text-2.0`

## [v1.4.5.1](https://github.com/diagrams/diagrams-lib/tree/v1.4.5.1) (2021-12-17)

- Bug fix: make things compile again under versions of `lens` before 5.1.

## [v1.4.5](https://github.com/diagrams/diagrams-lib/tree/v1.4.5) (2021-12-16)

- Allow `base-4.16` (tested with GHC 9.2.1)
- Allow `semigroups-0.20`, `lens-5.1`, `hashable-1.4`,
  `transformers-0.6`
- Add `Eq` instance for `SizeSpec`

## [v1.4.4-r1](https://github.com/diagrams/diagrams-lib/tree/v1.4.4-r1) (2021-06-05)

- Bumps to dependency upper bounds:
    - Allow `tasty-1.4`
    - Allow `bytestring-0.11`

## [v1.4.4](https://github.com/diagrams/diagrams-lib/tree/v1.4.4) (2021-05-24)

- Bumps to upper bounds, to allow building with:
    - `base-4.15` (tested with GHC 9.0.1)
    - `optparse-applicative` (tested with GHC 8.8.4 & 8.10.2)

- Updated use of Kinds thoughout the package

- Drop support for GHC 8.2 or earlier

## [v1.4.3](https://github.com/diagrams/diagrams-lib/tree/v1.4.3) (2019-11-06)

- Bumps to upper bounds, to allow building with:
    - `base-4.13` (tested with GHC 8.8.1)
    - `intervals-0.9`
    - `semigroups-0.19`
    - `hashable-1.3`
- Many bug fixes, including
    - [#313](https://github.com/diagrams/diagrams-lib/issues/313) (`combineBoundaries`)
    - [#322](https://github.com/diagrams/diagrams-lib/issues/322), [#329](https://github.com/diagrams/diagrams-lib/issues/329) (`section`)
    - [#325](https://github.com/diagrams/diagrams-lib/pull/325)
      (Bezier/Bezier intersection)
    - [#339](https://github.com/diagrams/diagrams-lib/pull/339) (`perspectiveZ1`)
- Added derived `Eq` and `Ord` instances for `FixedSegment`

## [v1.4.2.3](https://github.com/diagrams/diagrams-lib/tree/v1.4.2.3) (2018-06-11)

- Bug fix for `extrudeEnvelope` and friends ([#316](https://github.com/diagrams/diagrams-lib/issues/316))

## [v1.4.2.2](https://github.com/diagrams/diagrams-lib/tree/v1.4.2.2) (2018-05-08)

- Fixes for GHC < 8.0

## [v1.4.2.1](https://github.com/diagrams/diagrams-lib/tree/v1.4.2.1) (2018-04-13)

- Allow `base-4.11` (GHC 8.4)
- Allow `tasty-quickcheck-0.10`
- Bug fix for `Diagrams.TwoD.Offset.capArc` ([#310](https://github.com/diagrams/diagrams-lib/pull/310))

## [v1.4.2-r1](https://github.com/diagrams/diagrams-lib/tree/v1.4.2-r1) (2017-12-20)

Hackage revision to allow `tasty-1.0` in the test suite.

## [v1.4.2](https://github.com/diagrams/diagrams-lib/tree/v1.4.2) (2017-12-20)

- New functions:
    - `boxGrid`, for computing a grid of regularly spaced points.
    - `scalingRotationTo` and `scaleRotateTo`, for affine conformal 2D
      transformations.
- Documentation fixes:
    - `dirBetween`
    - `PolyOrientation`
- Upper bound updates: allow `tasty-0.12`, `tasty-quickcheck-0.9`,
  `tasty-hunit-0.10`, `optparse-applicative-0.14`
- Test with GHC 8.2.2

## [v1.4.1.2](https://github.com/diagrams/diagrams-lib/tree/v1.4.1.2) (2017-06-10)

- Fix test suite compilation failure [#299](https://github.com/diagrams/diagrams-lib/issues/299).

## [v1.4.1.1](https://github.com/diagrams/diagrams-lib/tree/v1.4.1.1) (2017-06-06)

- Fix `Diagrams.Points.centroid` to make it total.
- Fix bug in `Diagrams.Transform.Matrix.fromMatWithInv` (and hence
  also related functions which called it, such as `fromMat22` and
  `fromMat33`).

## [v1.4.1](https://github.com/diagrams/diagrams-lib/tree/v1.4.1) (2017-05-28)

- New functions `embeddedImage` and `loadImageEmbBS` for loading
  images.
- Fix [#289](https://github.com/diagrams/diagrams-lib/issues/289)
  which could have caused strange behavior in looped compilation mode
  on 32-bit platforms.
- Allow `intervals-0.8` and `directory-1.3`.
- Minor fixes to compile with GHC 8.2.

## [v1.4.0.1](https://github.com/diagrams/diagrams-lib/tree/v1.4.0.1) (2016-11-07)

- Fix test suite compilation problem ([#286](https://github.com/diagrams/diagrams-lib/issues/286))

## [v1.4](https://github.com/diagrams/diagrams-lib/tree/v1.4) (2016-10-26)

* **New features**

    - New `mkText'` function, which allows making a text primitive
      without recommending a fill colour or font size so users can
      recommend their own (*e.g.* using the `recommendFontSize`
      function).

    - New functions `reflectXY` and `reflectionXY`

    - New `composeAligned` combinator for doing composition under an
      alignment while leaving the local origin unaffected.

    - Add `_LocLoop` and `_LocLine` prisms

    - New `bspline` function for creating uniform cubic B-splines

    - New 3D features:
        - New `Skinned` class
        - Improved handling of 3D primitives
        - CSG

    - New standard attributes for separate fill and stroke opacity
      (see
      [#248](https://github.com/diagrams/diagrams-lib/issues/248)).

    - New `HasQuery` class for things which can be spatially queried
      for values from some monoid.

    - New function `inquire` for testing whether a given point is
      inside a diagram.

    - New font weights: `bolder`, `lighter`, `thinWeight`,
      `ultraLight`, `light`, `mediumWeight`, `heavy`, `semiBold`,
      `ultraBold`.  Note that currently only the SVG backend deals
      with the new weights.

    - Export `GetSegmentCodomain` and update documentation

    - Improved performance of 2D rotations

* **New instances**

    - `Alignable` instance for `Located`

    - `ToPath` instances for lines and loops

    - `Serialize` instances for `Trail`, `Path`, `Located`, `SegTree`,
      `Segment`

    - `Generic` instances for `Path`, `Located`

    - `Action` instance for `Angle`: angles act by rotation.

* **API changes**

    - `snugBL`, `snugBR`, `snugTR` and `snugBR` are deprecated.
      These functions were unintuitive, ad-hoc, and not particularly useful,
      especially since e.g. `snugL` and `snugB` do not commute. You
      can use something like `snugB . snugL` directly, or use `snug`
      with a direction vector.  See
      [#250](https://github.com/diagrams/diagrams-lib/issues/250) for
      more details.

* **Dependency/version changes**

    - upgrade `fsnotify` and drop dependency on deprecated
      `system-filepath`
    - Allow `lens-4.15`
    - Many other bumped upper bounds, see release notes for minor releases
      below

* **Bug fixes**

    - fix `orientPoints` function, which was previously generating NaN
      values with lists of only one or two
      points. ([#210](https://github.com/diagrams/diagrams-lib/issues/210))

    - Broken offset joins with non-vertices in loops ([#263](https://github.com/diagrams/diagrams-lib/issues/263))

    - Properly transform arrow shaft styles ([#274](https://github.com/diagrams/diagrams-lib/issues/274))

    - Fix sign error in `reflectionAbout`

## [v1.3.1.4](https://github.com/diagrams/diagrams-lib/tree/v1.3.1.4) (2016-08-16) (16 August 2016)

- allow `optparse-applicative-0.13`

## [v1.3.1.3](https://github.com/diagrams/diagrams-lib/tree/v1.3.1.3) (2016-06-05)

- allow `base-4.9`
- allow `data-default-class-0.1`
- test with GHC 8.0.1

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.3.1.2...v1.3.1.3)

## [v1.3.1.2](https://github.com/diagrams/diagrams-lib/tree/v1.3.1.2) (2016-05-01)

- allow `lens-4.14`

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.3.1.1...v1.3.1.2)

## [v1.3.1.2](https://github.com/diagrams/diagrams-lib/tree/v1.3.1.2) (2016-02-19)

  - allow `unordered-containers-0.2.*`

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.3.1.0...v1.3.1.1)

## [v1.3.1.0](https://github.com/diagrams/diagrams-lib/tree/v1.3.1.0) (2016-02-14)

- improve path offset calculations

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.3.0.9...v1.3.1.0)

## [v1.3.0.9](https://github.com/diagrams/diagrams-lib/tree/v1.3.0.9) (2016-01-14)

- allow `unordered-containers-0.2.6`

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.3.0.8...v1.3.0.9)

## [v1.3.0.8](https://github.com/diagrams/diagrams-lib/tree/v1.3.0.8) (2015-11-10)

- allow `semigroups-0.18`

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.3.0.7...v1.3.0.8)

## [v1.3.0.7](https://github.com/diagrams/diagrams-lib/tree/v1.3.0.7) (2015-10-08) ##

- Add `LambdaCase` extension to `.cabal` file, so `cabal` correctly
  reports that `diagrams-lib` does not build on `GHC-7.4`.

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.3.0.6...v1.3.0.7)

## [v1.3.0.6](https://github.com/diagrams/diagrams-lib/tree/v1.3.0.6) (2015-09-29) ##

- Allow `optparse-applicative-0.12`

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.3.0.5...v1.3.0.6)

## [v1.3.0.5](https://github.com/diagrams/diagrams-lib/tree/v1.3.0.5) (2015-09-18) ##

- Fix compilation problem with `lens-4.13`

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.3.0.4...v1.3.0.5)

## [v1.3.0.4](https://github.com/diagrams/diagrams-lib/tree/v1.3.0.4) (2015-09-17) ##

**Dependency/version changes**

- Allow `lens-1.13`
- Allow `semigroups-0.17`
- Require `linear-1.20`

## [v1.3.0.3](https://github.com/diagrams/diagrams-lib/tree/v1.3.0.3) (2015-08-10)

**Dependency/version changes**

- Drop dependency on deprecated `system-filepath` package
- Require `fsnotify-0.2.1`

## [v1.3.0.2](https://github.com/diagrams/diagrams-lib/tree/v1.3.0.2) (2015-07-19)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.3.0.1...v1.3.0.2)

## [v1.3.0.1](https://github.com/diagrams/diagrams-lib/tree/v1.3.0.1) (2015-05-26)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.3...v1.3.0.1)

## [v1.3](https://github.com/diagrams/diagrams-lib/tree/v1.3) (2015-04-19)

**New features**

- Native image type that backends can specify.

- Affine maps between spaces for path-like objects. A new
  `Diagrams.ThreeD.Projections` has some helper functions for
  orthographic and perspective projections.

- Intersections for path-like objects using Bézier clipping.

- Helper functions in `Diagrams.Matrix` for converting between
  transforms and matrices.

- New `Diagrams` module that only exports functions defined in
  diagrams.

- New `Direction` type. `Direction` is a vector that's forgot it's
  magnitude. Some functions have changed their type from `R2` to
  `Direction V2 n` to make it clear that magnitude has no effect.

- Use the [`fsnotify`](https://hackage.haskell.org/package/fsnotify)
  package for command line looping. Command line looping now works
  on Windows.

- `groupOpacity` function added for lowering the opacity of a
  diagram as a whole.

- New `ToPath` class for converting path-like things to a `Path`.

**New instances**

- `Each` instances for `BoundingBox`, `Offset`, `Segment`,
  `FixedSegment` and `Path`.

- `Reversing` instances for `Offset`, `Segment`, `FixedSegment`,
  `Trail` and `Path`.

- `AsEmpty` instances for `BoundingBox`, `Trail` and `Path`.

- `Cons` and `Snoc` instances for `Path` and `Line`.

- New `Show` instances for `Angle`, `Segment`, `SomeColor`, `Trail'`
  and `at`.

- `Tangent` instance for `(FixedSegment v n)`.

- `Ord` instances for `LineMiterLimit`, `LineJoin` and `LineCap`.

**New helper functions**

- `_Line` and `_Loop` prisms.

- Style lenses: `_fontSize`, `_lineWidth`, `_fillTexture`,
  `_lineTexture`, `_opacity`, `_font`, `_lineCap`, `_lineJoin`
  `_dashing`.

- `_SomeColor` iso and `_AC` prism onto an `AlphaColour`.

- `atPoints` function to zip points with diagrams.

**API changes**

- `Diagram` type synonym now only takes a backend token: `Diagram B`

- Types that previously had a `v` variable now have `v` and `n`.

- `Control.Lens` and `Data.Default.Class` are now exported from from
  `Diagrams.Prelude`

- `Measure` has a new internal representation. `Local`, `Global`,
  `Normalized`, and `Output` have been renamed to `local`, `global`,
  `normalized` and `output` respectivly.

- `SizeSpec2D` has moved to `SizeSpec v n` in `Diagrams.SizeSpec`.
  `Dims, Height, Width and `Absolute` have moved to `dims2D`,
  `mkHeight`, `mkWidth` and `absolute` respectively.

- `Color` instances for `Colour` and `AlphaColour` are limited to
  `Double` for better type inference.

- `under` has been renamed to `underT`. New `transformed`,
  `translated`, `movedTo`, `movedFrom` and `rotated` isomorphisms to
  use with lens's `under` function.

- `stroke` is now polymorphic. Use `strokePath` or `strokeP` to get
  old `stroke` behaviour.

- `angleBetween` now works for any vector space, which means the
  angle is always positive. The old behaviour can be retrieved from
  `signedAngleBetween`

- `arc` now takes a starting `Direction` and a sweep `Angle`.
  `arcCW` and `arcCCW` take a start and finish `Direction`.

**Dependency/version changes**

- use `linear` instead of `vector-space`

**Closed issues:**

- Perspective deformation of square vertices yields extra point [\#244](https://github.com/diagrams/diagrams-lib/issues/244)

- Local fontsize renders inconsistentl on diffrent backends [\#243](https://github.com/diagrams/diagrams-lib/issues/243)

- Factor out Diagrams.Solve into a package? [\#235](https://github.com/diagrams/diagrams-lib/issues/235)

**Merged pull requests:**

- add pathPoints and pathVertices' functions [\#245](https://github.com/diagrams/diagrams-lib/pull/245) ([byorgey](https://github.com/byorgey))

- New loop [\#242](https://github.com/diagrams/diagrams-lib/pull/242) ([cchalmers](https://github.com/cchalmers))

- Pre 1.3 [\#241](https://github.com/diagrams/diagrams-lib/pull/241) ([cchalmers](https://github.com/cchalmers))

- Update CubicSpline.hs [\#240](https://github.com/diagrams/diagrams-lib/pull/240) ([fryguybob](https://github.com/fryguybob))

- updated changes for GHC-7.10 [\#239](https://github.com/diagrams/diagrams-lib/pull/239) ([bergey](https://github.com/bergey))

- split out new package diagrams-solve [\#237](https://github.com/diagrams/diagrams-lib/pull/237) ([byorgey](https://github.com/byorgey))

- Lens style [\#236](https://github.com/diagrams/diagrams-lib/pull/236) ([cchalmers](https://github.com/cchalmers))

- Half-dart arrowheads [\#234](https://github.com/diagrams/diagrams-lib/pull/234) ([byorgey](https://github.com/byorgey))

- TwoD.Points: Needs TypeFamilies [\#233](https://github.com/diagrams/diagrams-lib/pull/233) ([bgamari](https://github.com/bgamari))

- Projections [\#229](https://github.com/diagrams/diagrams-lib/pull/229) ([cchalmers](https://github.com/cchalmers))

## [1.2.0.9]() (2 April 2015)

 - allow `lens-4.9`

 - allow `vector-space-0.10`

## [v1.2.0.8](https://github.com/diagrams/diagrams-lib/tree/v1.2.0.8) (2015-01-13)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.2.0.7...v1.2.0.8)

**Merged pull requests:**

- typo in haddocks [\#232](https://github.com/diagrams/diagrams-lib/pull/232) ([ggreif](https://github.com/ggreif))

- Update diagrams-lib.cabal : bumping JuicyPixels dependency [\#230](https://github.com/diagrams/diagrams-lib/pull/230) ([Twinside](https://github.com/Twinside))

## [v1.2.0.7](https://github.com/diagrams/diagrams-lib/tree/v1.2.0.7) (2014-12-07)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.2.0.6...v1.2.0.7)

## [v1.2.0.6](https://github.com/diagrams/diagrams-lib/tree/v1.2.0.6) (2014-12-04)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.2.0.5...v1.2.0.6)

**Merged pull requests:**

- Minor changes in documentation about Polygons. [\#228](https://github.com/diagrams/diagrams-lib/pull/228) ([alexDarcy](https://github.com/alexDarcy))

## [v1.2.0.5](https://github.com/diagrams/diagrams-lib/tree/v1.2.0.5) (2014-11-17)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.2.0.4...v1.2.0.5)

**Implemented enhancements:**

- Path/trail intersection [\#209](https://github.com/diagrams/diagrams-lib/issues/209)

- Turn R2 into D2 \(Generalize R2 to any numeric type\) [\#50](https://github.com/diagrams/diagrams-lib/issues/50)

**Fixed bugs:**

- `Sectionable` instance for `SegTree` is not a linear reparameterization. [\#217](https://github.com/diagrams/diagrams-lib/issues/217)

**Closed issues:**

- bezierFromSweep very slow? [\#227](https://github.com/diagrams/diagrams-lib/issues/227)

- All lines render at same width [\#222](https://github.com/diagrams/diagrams-lib/issues/222)

- numerically stable cubic rootfinder [\#204](https://github.com/diagrams/diagrams-lib/issues/204)

**Merged pull requests:**

- Intersections [\#226](https://github.com/diagrams/diagrams-lib/pull/226) ([cchalmers](https://github.com/cchalmers))

- Linear update [\#225](https://github.com/diagrams/diagrams-lib/pull/225) ([cchalmers](https://github.com/cchalmers))

- Add arcCCW and friends.  Fix offset joins. [\#221](https://github.com/diagrams/diagrams-lib/pull/221) ([fryguybob](https://github.com/fryguybob))

- Reparameterize section [\#220](https://github.com/diagrams/diagrams-lib/pull/220) ([fryguybob](https://github.com/fryguybob))

- Diagram b v n to QDiagram b v n Any [\#219](https://github.com/diagrams/diagrams-lib/pull/219) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- New stuff [\#218](https://github.com/diagrams/diagrams-lib/pull/218) ([cchalmers](https://github.com/cchalmers))

- Linear [\#215](https://github.com/diagrams/diagrams-lib/pull/215) ([cchalmers](https://github.com/cchalmers))

## [v1.2.0.4](https://github.com/diagrams/diagrams-lib/tree/v1.2.0.4) (2014-10-08)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.2.0.3...v1.2.0.4)

**Merged pull requests:**

- add semi-portable looping using fsnotify [\#213](https://github.com/diagrams/diagrams-lib/pull/213) ([bergey](https://github.com/bergey))

## [v1.2.0.3](https://github.com/diagrams/diagrams-lib/tree/v1.2.0.3) (2014-09-07)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.2.0.2...v1.2.0.3)

**Closed issues:**

- arrowFromLocatedTrail function [\#205](https://github.com/diagrams/diagrams-lib/issues/205)

**Merged pull requests:**

- Added `arrowFromLocatedTrail` [\#206](https://github.com/diagrams/diagrams-lib/pull/206) ([pnutus](https://github.com/pnutus))

## [v1.2.0.2](https://github.com/diagrams/diagrams-lib/tree/v1.2.0.2) (2014-08-22)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.2.0.1...v1.2.0.2)

**Implemented enhancements:**

- Generalize Angle class? [\#38](https://github.com/diagrams/diagrams-lib/issues/38)

**Fixed bugs:**

- --selection and --src both use -s abbreviation [\#172](https://github.com/diagrams/diagrams-lib/issues/172)

**Closed issues:**

- Spike arrowhead and tail render with oversized joint [\#203](https://github.com/diagrams/diagrams-lib/issues/203)

**Merged pull requests:**

- Enable compilation with GHC HEAD \(v7.9\) [\#211](https://github.com/diagrams/diagrams-lib/pull/211) ([ggreif](https://github.com/ggreif))

- cli-options [\#200](https://github.com/diagrams/diagrams-lib/pull/200) ([bergey](https://github.com/bergey))

- Add Native images to Diagrams.TwoD.Image [\#199](https://github.com/diagrams/diagrams-lib/pull/199) ([taruti](https://github.com/taruti))

- add atPoints, deprecate decorateFoo [\#198](https://github.com/diagrams/diagrams-lib/pull/198) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Add bg' [\#197](https://github.com/diagrams/diagrams-lib/pull/197) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- add convenience synonyms \[hv\]sep for \[hv\]cat' \(with & sep .~ x\) [\#196](https://github.com/diagrams/diagrams-lib/pull/196) ([byorgey](https://github.com/byorgey))

- Vertices [\#192](https://github.com/diagrams/diagrams-lib/pull/192) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Direction type [\#186](https://github.com/diagrams/diagrams-lib/pull/186) ([bergey](https://github.com/bergey))

## [v1.2.0.1](https://github.com/diagrams/diagrams-lib/tree/v1.2.0.1) (2014-06-04)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.2...v1.2.0.1)

**Closed issues:**

- Local headLength / headGaps act like Output [\#193](https://github.com/diagrams/diagrams-lib/issues/193)

**Merged pull requests:**

- transform Local Measure in arrow styles [\#194](https://github.com/diagrams/diagrams-lib/pull/194) ([bergey](https://github.com/bergey))

## [v1.2](https://github.com/diagrams/diagrams-lib/tree/v1.2) (2014-06-02)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.1.0.4...v1.2)

**New features**

- Several attributes (such as line width, dashing size, arrowhead
  size, and font size) that formerly had a value of type `Double`
  now have the more general type `Measure R2`. This allows the
  attributes to be specified relative to one of four measurement
  frames: `Local`, `Global`, `Normalized`, and `Output`.

- New DSL for specifying measurements.

- New synonyms for specifying measurements, *e.g.* `thin`,
  `thick`, and `medium`, `large`.

- Support for radial and linear gradients for fills and strokes.

- New `DImage` type that supports both embedded and external images in
  Backends that support them.

- New `lengths` Traversal for setting `headLength` and `tailLength`
  simultaneously.

- `Frustrum` and `Box` shapes added to `Diagrams.ThreeD.Shapes`.

- New function `quartForm` to find roots of quartic polynomials.

- New Lenses for polar coordinates.

- New trig functions, `sinA`, `atanA`, etc. which take `Angle` as
  input or output.

**New instances**

- `Transformable` instances for `LineWidth`, `Dashing`,
  `LineTexture`, and  `FillTexture`.

**API changes**

- `FillColor` and `LineColor` attributes have been replaced with
  the more general `FillTexture` and `LineTexture`. Use the `solid`
  function to convert a color to a texture.

- The size of arrow heads and tails is now specified in terms of
  length instead of the radius of their circumcircle.

- Gaps at the ends of arrows are now specified using `Measure R2`.

- The `gap` traversal has been replaced by `gaps` for consistency
  in naming, though `gap` is still provided for backwards compatibility.

- `fontSize` now takes a value of type `Measure R2`.

- Get rid of (bitrotted) `Show` backend.

- Functions in `TwoD.Adjust` now return the adjustment
  transformation itself in addition to the resized `Diagram` and
  `Options` record; this can be used, *e.g.* to convert screen
  coordinates back into diagram coordinates.

- Export `pathLocSegments`.

- The `avgScale` function has been moved to `Diagrams.Core`.

- The `Angle` definition and related functions (*e.g.*
  `angleBetween`) have moved to a separate module, `Diagrams.Angle`.

- A separate `Diagrams.TwoD.Attributes` module now contains most
  of the attributes that require 2D transformation instances.

- The `splitColorFills` function has been replaced by `splitTextureFills`.

**Dependency/version changes**

- Allow `semigroups-0.15`

- Allow `opt-parse-applicative-0.9.0

- Allow `lens-4.2`

**Implemented enhancements:**

- 'image' should be in IO [\#29](https://github.com/diagrams/diagrams-lib/issues/29)

- Add gradient support [\#9](https://github.com/diagrams/diagrams-lib/issues/9)

**Closed issues:**

- fails to build against HP [\#190](https://github.com/diagrams/diagrams-lib/issues/190)

- text does not scale [\#179](https://github.com/diagrams/diagrams-lib/issues/179)

- Please add support for latest version of intervals library [\#170](https://github.com/diagrams/diagrams-lib/issues/170)

- presence of arrowHead can tilt connection sideways [\#162](https://github.com/diagrams/diagrams-lib/issues/162)

**Merged pull requests:**

- Quartic formula, no obvious bugs [\#187](https://github.com/diagrams/diagrams-lib/pull/187) ([Mathnerd314](https://github.com/Mathnerd314))

- Arrow length [\#185](https://github.com/diagrams/diagrams-lib/pull/185) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- fix handling of text scaling [\#182](https://github.com/diagrams/diagrams-lib/pull/182) ([byorgey](https://github.com/byorgey))

- Texture [\#181](https://github.com/diagrams/diagrams-lib/pull/181) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- arrow envelopes [\#180](https://github.com/diagrams/diagrams-lib/pull/180) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- make headSize and tailSize back into ArrowOpts fields [\#177](https://github.com/diagrams/diagrams-lib/pull/177) ([byorgey](https://github.com/byorgey))

- updated the upper bounds of the .cabal constraints for 'semigroups' package [\#176](https://github.com/diagrams/diagrams-lib/pull/176) ([zgredzik](https://github.com/zgredzik))

- Image2 [\#174](https://github.com/diagrams/diagrams-lib/pull/174) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Image [\#173](https://github.com/diagrams/diagrams-lib/pull/173) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Preliminary implementation of Measure [\#159](https://github.com/diagrams/diagrams-lib/pull/159) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Gradient [\#136](https://github.com/diagrams/diagrams-lib/pull/136) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v1.1.0.7]() (2014-05-15)

- Allow `semigroups-0.14`

## [v1.1.0.6]() (2014-04-10)

- Allow `semigroups-0.13`

## [v1.1.0.4](https://github.com/diagrams/diagrams-lib/tree/v1.1.0.4) (2014-04-04)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.1.0.2...v1.1.0.4)

**Merged pull requests:**

- Updates to work with `Backend` redesign [\#171](https://github.com/diagrams/diagrams-lib/pull/171) ([byorgey](https://github.com/byorgey))

- Rework of units [\#169](https://github.com/diagrams/diagrams-lib/pull/169) ([byorgey](https://github.com/byorgey))

- Make Diagrams.Transform.under more polymorphic [\#168](https://github.com/diagrams/diagrams-lib/pull/168) ([FlorentBecker](https://github.com/FlorentBecker))

## [v1.1.0.3]() (2014-03-19)

- Allow `lens-4.1`

## [v1.1.0.2](https://github.com/diagrams/diagrams-lib/tree/v1.1.0.2) (2014-03-19)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.1.0.1...v1.1.0.2)

**Merged pull requests:**

- move avgScale to core [\#167](https://github.com/diagrams/diagrams-lib/pull/167) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- 3d color & lighting, more solids, Angle utilities [\#166](https://github.com/diagrams/diagrams-lib/pull/166) ([bergey](https://github.com/bergey))

- Added bothSize function, lineHead and lineTail [\#165](https://github.com/diagrams/diagrams-lib/pull/165) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v1.1.0.1](https://github.com/diagrams/diagrams-lib/tree/v1.1.0.1) (2014-03-09)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.1...v1.1.0.1)

- Depend on `hashable` package, and add `Hashable` instance for
  `SizeSpec2D`.

  Technically, the PVP specifies that adding a new instance
  requires a major version bump.  However, I highly doubt anyone
  was making their own orphan `Hashable` instances before.  Feel
  free to yell at Brent if this breaks your build.

## [v1.1](https://github.com/diagrams/diagrams-lib/tree/v1.1) (2014-03-09)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.0.1...v1.1)

**New features**

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

**New instances**

- `Typeable` instances for all data types that are used as diagram
  primitives.

- `Sectionable` instance for `FixedSegment`.

**API changes**

- `Angle` is now a type, rather than a class.  It uses a single
  internal representation for angles, and lenses `turn`, `rad,`
  and `deg` are supplied for constructing (using `@@`) and viewing
  (using `^.`) `Angle`s in various units.  In addition, the `Num`
  instance for `Angle` has been removed, eliminating a class of
  errors where a bare number is interpreted in units other than
  what you expect.

- Removed `Num` instance for angles.

**Dependency/version changes**

- Require `lens >= 4.0`.

- Allow `array-0.5`.

- Allow `hashable-1.1`.

- Remove `NumInstances` dependency.

**Bug fixes**

- Exclude joins in offsets on close segments (#160).

- Exclude extra segment when joining loops in offset (#155).

**Performance improvements**

- `colorToSRGBA` function now avoids expensive matrix operations,
  offering dramatic speedups in rendering diagrams with many color
  attributes.

**Implemented enhancements:**

- Better color model in 3D [\#121](https://github.com/diagrams/diagrams-lib/issues/121)

- Projective/perspective transforms for points and paths [\#108](https://github.com/diagrams/diagrams-lib/issues/108)

- clipTo function [\#35](https://github.com/diagrams/diagrams-lib/issues/35)

**Fixed bugs:**

- Offset Bug [\#155](https://github.com/diagrams/diagrams-lib/issues/155)

**Closed issues:**

- Expand of an expand [\#160](https://github.com/diagrams/diagrams-lib/issues/160)

**Merged pull requests:**

- fix doc for === and ||| [\#164](https://github.com/diagrams/diagrams-lib/pull/164) ([denys-duchier](https://github.com/denys-duchier))

- Split fills [\#161](https://github.com/diagrams/diagrams-lib/pull/161) ([byorgey](https://github.com/byorgey))

- Exclude extra segment when joining loops in offset. Fixes \#155. [\#158](https://github.com/diagrams/diagrams-lib/pull/158) ([fryguybob](https://github.com/fryguybob))

- added basis, generalized `centerXY` and `snugXY` [\#157](https://github.com/diagrams/diagrams-lib/pull/157) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- derive Typeable for all data types used in Prims [\#156](https://github.com/diagrams/diagrams-lib/pull/156) ([byorgey](https://github.com/byorgey))

- Lens4 [\#154](https://github.com/diagrams/diagrams-lib/pull/154) ([bergey](https://github.com/bergey))

- removed NumInstances dependency [\#153](https://github.com/diagrams/diagrams-lib/pull/153) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Remove Num instance for Angle [\#150](https://github.com/diagrams/diagrams-lib/pull/150) ([bergey](https://github.com/bergey))

- Change internal color representation to SRGBA [\#149](https://github.com/diagrams/diagrams-lib/pull/149) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Projections - non-affine transformations [\#148](https://github.com/diagrams/diagrams-lib/pull/148) ([bergey](https://github.com/bergey))

## [v1.0.1](https://github.com/diagrams/diagrams-lib/tree/v1.0.1) (2014-01-26)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.0.0.1...v1.0.1)

**Implemented enhancements:**

- Module for generating nice colors/color schemes [\#77](https://github.com/diagrams/diagrams-lib/issues/77)

**Closed issues:**

- "Maybe.fromJust: Nothing" error when connecting translated small diagram [\#147](https://github.com/diagrams/diagrams-lib/issues/147)

**Merged pull requests:**

- Add Hashable instance for SizeSpec2D [\#146](https://github.com/diagrams/diagrams-lib/pull/146) ([byorgey](https://github.com/byorgey))

- return list of traces [\#145](https://github.com/diagrams/diagrams-lib/pull/145) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- added clipTo [\#144](https://github.com/diagrams/diagrams-lib/pull/144) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Unified angle type [\#140](https://github.com/diagrams/diagrams-lib/pull/140) ([bergey](https://github.com/bergey))

## [v1.0.0.1](https://github.com/diagrams/diagrams-lib/tree/v1.0.0.1) (2013-11-28)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.0...v1.0.0.1)

## [v1.0](https://github.com/diagrams/diagrams-lib/tree/v1.0) (2013-11-25)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v0.7.1.1...v1.0)

**New features**

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

**New instances**

- `AdditiveGroup` and `VectorSpace` instances for `Turn`, `Rad`, `Deg`

- `Alignable` instance for `(->) e`

- `Default` instances for `FillRule`, `FillRuleA`, `LineJoin`,
    `LineCap`, `FillColor`

- `Show` instances for `FillRule`, `FillRuleA`

**API changes**

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

**Dependency/version changes**

- Remove `newtype` dependency

- New dependencies on `lens`, `tagged`, `optparse-applicative`,
  `filepath`, `safe`, `vector-space-points`, `MemoTrie`

- Depend on `intervals >= 0.3 && < 0.5`.

**Bug fixes**

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

- Arrows do not behave correctly under scaling [\#112](https://github.com/diagrams/diagrams-lib/issues/112)

**Performance improvements**

- `R2` is now strict and `UNPACK`ed

- Add strictness to `Offset`, `Segment`, `OffsetEnvelope`, and `SizeSpec2D`.

- Make `getEnvelope` calculation for `Segment` more efficient by
  floating divisions out of the inner calculation.

- Use a specialized `HasTrie` instance for `R2`.


**Closed issues:**

- diagrams-lib-0.7.1.1 fails to build with ghc-7.7 \(-HEAD\) [\#128](https://github.com/diagrams/diagrams-lib/issues/128)

- Arrow misses target [\#126](https://github.com/diagrams/diagrams-lib/issues/126)

**Merged pull requests:**

- Lenses for setting arrow head, tail, and shaft colors [\#138](https://github.com/diagrams/diagrams-lib/pull/138) ([byorgey](https://github.com/byorgey))

- Delayed subtrees [\#137](https://github.com/diagrams/diagrams-lib/pull/137) ([byorgey](https://github.com/byorgey))

- Add helpers for common optparse-applicative backend command line. [\#135](https://github.com/diagrams/diagrams-lib/pull/135) ([fryguybob](https://github.com/fryguybob))

- add located lens for access into Located things [\#134](https://github.com/diagrams/diagrams-lib/pull/134) ([byorgey](https://github.com/byorgey))

- add b-\>a instance for Alignable [\#133](https://github.com/diagrams/diagrams-lib/pull/133) ([cscheid](https://github.com/cscheid))

- Strictness optimizations [\#132](https://github.com/diagrams/diagrams-lib/pull/132) ([JohnLato](https://github.com/JohnLato))

- Lens [\#131](https://github.com/diagrams/diagrams-lib/pull/131) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- 3D scaling, alignment, coordinate lenses [\#129](https://github.com/diagrams/diagrams-lib/pull/129) ([bergey](https://github.com/bergey))

- correctly \(?\) compute shaftScale by solving a quadratic [\#127](https://github.com/diagrams/diagrams-lib/pull/127) ([byorgey](https://github.com/byorgey))

- calculate arrow shaftScale using projection of ends onto shaft offset [\#125](https://github.com/diagrams/diagrams-lib/pull/125) ([bergey](https://github.com/bergey))

- Convert from newtype to lens [\#124](https://github.com/diagrams/diagrams-lib/pull/124) ([byorgey](https://github.com/byorgey))

- Version bump on lens [\#123](https://github.com/diagrams/diagrams-lib/pull/123) ([haasn](https://github.com/haasn))

## [v0.7.1.1](https://github.com/diagrams/diagrams-lib/tree/v0.7.1.1) (2013-09-27)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v0.7.1...v0.7.1.1)

**Implemented enhancements:**

- Use a point envelope for built-in text objects [\#116](https://github.com/diagrams/diagrams-lib/issues/116)

- implement rotations in 3D [\#86](https://github.com/diagrams/diagrams-lib/issues/86)

- Control over boundary conditions on cubic splines. [\#32](https://github.com/diagrams/diagrams-lib/issues/32)

**Fixed bugs:**

- diagrams with empty envelopes are not properly separated by struts [\#115](https://github.com/diagrams/diagrams-lib/issues/115)

**Closed issues:**

- calculate ratio between two angles [\#109](https://github.com/diagrams/diagrams-lib/issues/109)

- Add primitives for arrows [\#73](https://github.com/diagrams/diagrams-lib/issues/73)

**Merged pull requests:**

- Three d render [\#114](https://github.com/diagrams/diagrams-lib/pull/114) ([bergey](https://github.com/bergey))

- API for computing tangent and normal vectors to segments and trails [\#113](https://github.com/diagrams/diagrams-lib/pull/113) ([byorgey](https://github.com/byorgey))

- change default styles for arrows [\#111](https://github.com/diagrams/diagrams-lib/pull/111) ([byorgey](https://github.com/byorgey))

- 3D utility functions [\#107](https://github.com/diagrams/diagrams-lib/pull/107) ([bergey](https://github.com/bergey))

## [v0.7.1](https://github.com/diagrams/diagrams-lib/tree/v0.7.1) (2013-09-11)

**New features**

- New standard miter limit attribute

- New functions `lineColorA`, `lineWidthA`, `lineMiterLimitA`,
  `fontSizeA` for directly applying attribute values

- `setDefault2DAttributes` now sets default line cap (butt), line
  join (miter), and miter limit (10) attributes

**New instances**

- `Data.Default` instances for

- `LineCap`

- `LineJoin`

- `LineMiterLimit`

- `LineWidth`

- `LineColor`

- `FontSize`

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v0.7...v0.7.1)

**Implemented enhancements:**

- Path expansion. [\#55](https://github.com/diagrams/diagrams-lib/issues/55)

- way to move diagrams closer together [\#18](https://github.com/diagrams/diagrams-lib/issues/18)

**Fixed bugs:**

- wedge should be closed [\#99](https://github.com/diagrams/diagrams-lib/issues/99)

- Adjusting trail from the end has no effect [\#95](https://github.com/diagrams/diagrams-lib/issues/95)

**Merged pull requests:**

- Add offsetTrail and expandTrail. [\#103](https://github.com/diagrams/diagrams-lib/pull/103) ([fryguybob](https://github.com/fryguybob))

- change Codomain of Located to Point \(Codomain a\) [\#102](https://github.com/diagrams/diagrams-lib/pull/102) ([byorgey](https://github.com/byorgey))

- Default and Show instances for FillRule and FillRuleA [\#100](https://github.com/diagrams/diagrams-lib/pull/100) ([jbracker](https://github.com/jbracker))

- Changes required due to the introduction of Roles in GHC [\#98](https://github.com/diagrams/diagrams-lib/pull/98) ([co-dan](https://github.com/co-dan))

- removed default implementation of reverseDomain [\#97](https://github.com/diagrams/diagrams-lib/pull/97) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- fixed issue \#95 [\#96](https://github.com/diagrams/diagrams-lib/pull/96) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v0.7](https://github.com/diagrams/diagrams-lib/tree/v0.7) (2013-08-09)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v0.6.0.3...v0.7)

**New features**

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

**New instances**

- `IsPrim` instances for `Path`, `Ellipsoid`, `Image`, `Text`, and
  `ScaleInv`

- `Eq`, `Ord`, and `Show` instances for `SizeSpec2D`

**API changes**

- `CircleFrac` has been renamed `Turn` (though `CircleFrac` is
  retained as a deprecated synonym).

- `Diagrams.Coordinates` is no longer exported from
  `Diagrams.Prelude`.  This is for compatibility with `lens`, as `(&)`
  is a rather important lens operator and clashes with
  `Diagrams.Coordinates`.  Users who want the `Coordinates` stuff can import
  `Diagrams.Coordinates` explicitly.

**Dependency/version changes**

- allow `base-4.7`

- upgrade to `monoid-extras-0.3`

- depend on `data-default-class` instead of `data-default`

- Tested with GHC 7.7.

**Bug fixes**

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

**Implemented enhancements:**

- Split PathLike, Trail, and Path into separate modules [\#25](https://github.com/diagrams/diagrams-lib/issues/25)

- Add support for path-oriented diagrams. [\#13](https://github.com/diagrams/diagrams-lib/issues/13)

**Fixed bugs:**

- Wrong envelope for cubic segment [\#88](https://github.com/diagrams/diagrams-lib/issues/88)

- Fix documentation of beside, \(===\), and \(|||\) re: monoidal semantics [\#83](https://github.com/diagrams/diagrams-lib/issues/83)

- reverseTrail should not generate extra segments for closed trails [\#24](https://github.com/diagrams/diagrams-lib/issues/24)

**Closed issues:**

- Test issue [\#91](https://github.com/diagrams/diagrams-lib/issues/91)

**Merged pull requests:**

- Add parametric generalizations for segments, trails, Located, etc. [\#92](https://github.com/diagrams/diagrams-lib/pull/92) ([byorgey](https://github.com/byorgey))

- Adding type signature so it compiles with GHC7.7 [\#90](https://github.com/diagrams/diagrams-lib/pull/90) ([co-dan](https://github.com/co-dan))

- bug fix: add special case for b==0 to quadform solver [\#89](https://github.com/diagrams/diagrams-lib/pull/89) ([byorgey](https://github.com/byorgey))

- Big refactoring of segments and trails. [\#87](https://github.com/diagrams/diagrams-lib/pull/87) ([byorgey](https://github.com/byorgey))

- Stop exporting Diagrams.Coordinates from Diagrams.Prelude [\#82](https://github.com/diagrams/diagrams-lib/pull/82) ([byorgey](https://github.com/byorgey))

- Generalization of R2 to D2 a [\#65](https://github.com/diagrams/diagrams-lib/pull/65) ([jbracker](https://github.com/jbracker))

## [v0.6.0.3](https://github.com/diagrams/diagrams-lib/tree/v0.6.0.3) (2013-05-04)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v0.6.0.2...v0.6.0.3)

**Closed issues:**

- Offset [\#80](https://github.com/diagrams/diagrams-lib/issues/80)

**Merged pull requests:**

- Added offsetSegment function [\#79](https://github.com/diagrams/diagrams-lib/pull/79) ([fryguybob](https://github.com/fryguybob))

- Working on adding curvature. [\#74](https://github.com/diagrams/diagrams-lib/pull/74) ([fryguybob](https://github.com/fryguybob))

- New `scaleInvPrim` function [\#71](https://github.com/diagrams/diagrams-lib/pull/71) ([byorgey](https://github.com/byorgey))

## [v0.6.0.2](https://github.com/diagrams/diagrams-lib/tree/v0.6.0.2) (2013-03-29)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/types-generalized...v0.6.0.2)

**Implemented enhancements:**

- Don't export Eq and Show instances for functions from Diagrams.Prelude [\#48](https://github.com/diagrams/diagrams-lib/issues/48)

**Closed issues:**

- radius is wrong [\#75](https://github.com/diagrams/diagrams-lib/issues/75)

**Merged pull requests:**

- Matrix basis rep [\#78](https://github.com/diagrams/diagrams-lib/pull/78) ([bergey](https://github.com/bergey))

## [types-generalized](https://github.com/diagrams/diagrams-lib/tree/types-generalized) (2013-02-13)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v0.6.0.1...types-generalized)

## [v0.6.0.1](https://github.com/diagrams/diagrams-lib/tree/v0.6.0.1) (2013-01-07)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v0.6...v0.6.0.1)

**Fixed bugs:**

- text on zero width rect leads to nans [\#51](https://github.com/diagrams/diagrams-lib/issues/51)

- wrong result when using `beside` with circle and vector \(1 & \(-1\)\) [\#46](https://github.com/diagrams/diagrams-lib/issues/46)

- Handle `rect` with zero arguments. [\#43](https://github.com/diagrams/diagrams-lib/issues/43)

**Closed issues:**

- Generalization of color space used in rendering [\#66](https://github.com/diagrams/diagrams-lib/issues/66)

**Merged pull requests:**

- do scaling on a Path before constructing PathLike in 'rect' \(fixes \#43\) [\#70](https://github.com/diagrams/diagrams-lib/pull/70) ([byorgey](https://github.com/byorgey))

## [v0.6](https://github.com/diagrams/diagrams-lib/tree/v0.6) (2012-12-12)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/0_5_0_1...v0.6)

**New features**

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

**New instances**

- The `Show` instance for `R2` now produces something like `2 & 6`
  instead of `R2 { unR2 = (2,6) }`.  The `Read` instance has also
  been changed to match, so `read . show = id`.

- `Enveloped` instance for `FixedSegment`

- `Traced` instances for `Segment`, `FixedSegment`, `Trail`, and `Path`

- New derived `Eq` instances for `LineCapA`, `LineJoinA`, `Dashing`,
  `DashingA`, `FillRule`, `Font`, `FontSize`, `FontSlant`, `FontSlantA`,
  `FontWeight`, and `FontWeightA`

- `Renderable Ellipsoid NullBackend` instance

**API changes**

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

**Dependency/version changes**

- Allow `data-default` 0.4 and 0.5

- Allow `base`-4.6

- Allow `containers`-0.5

**Bug fixes**

- `arc` and `arcT` functions now always produce counterclockwise arcs,
  as claimed.

**Implemented enhancements:**

- Enhancements for `arc`s. [\#54](https://github.com/diagrams/diagrams-lib/issues/54)

- align and friends should be in terms of a new 'Alignable' class instead of 'Boundable' [\#31](https://github.com/diagrams/diagrams-lib/issues/31)

- Tools for more accurate boundary calculations in common cases [\#30](https://github.com/diagrams/diagrams-lib/issues/30)

- Reimplement Diagrams.TwoD.Ellipse in terms of Diagrams.TwoD.Arc [\#27](https://github.com/diagrams/diagrams-lib/issues/27)

- Function to convert angles into 2D unit vectors [\#23](https://github.com/diagrams/diagrams-lib/issues/23)

- 2D specialization of 'beside' which takes an angle instead of a vector [\#22](https://github.com/diagrams/diagrams-lib/issues/22)

- Generalize showOrigin function [\#21](https://github.com/diagrams/diagrams-lib/issues/21)

- Add generic 'extent' and 'breadth' ? functions [\#20](https://github.com/diagrams/diagrams-lib/issues/20)

- Add shearing transformations to standard library [\#19](https://github.com/diagrams/diagrams-lib/issues/19)

- Add support for text [\#15](https://github.com/diagrams/diagrams-lib/issues/15)

- Allow the user to choose the fill rule used for paths when stroking [\#14](https://github.com/diagrams/diagrams-lib/issues/14)

- Image primitives [\#10](https://github.com/diagrams/diagrams-lib/issues/10)

- Merge polygon code from Dmitry Olshansky [\#4](https://github.com/diagrams/diagrams-lib/issues/4)

**Fixed bugs:**

- Enhancements for `arc`s. [\#54](https://github.com/diagrams/diagrams-lib/issues/54)

- boundingBox computes incorrect bounding box for transformed diagrams [\#39](https://github.com/diagrams/diagrams-lib/issues/39)

- hcat is really \*terrible\* performance-wise [\#28](https://github.com/diagrams/diagrams-lib/issues/28)

- stroke sets the fill rule attribute to a default value, so it can't be changed later [\#26](https://github.com/diagrams/diagrams-lib/issues/26)

- Text alignment should be with respect to descent and ascent lines rather than text bounding box [\#17](https://github.com/diagrams/diagrams-lib/issues/17)

- incorrect bounds for Bezier segments [\#11](https://github.com/diagrams/diagrams-lib/issues/11)

- More combinators in D.Combinators [\#5](https://github.com/diagrams/diagrams-lib/issues/5)

**Merged pull requests:**

- clean up and fix bugs with ScaleInv wrapper [\#69](https://github.com/diagrams/diagrams-lib/pull/69) ([byorgey](https://github.com/byorgey))

- Patch proposal for generalized Color [\#67](https://github.com/diagrams/diagrams-lib/pull/67) ([haasn](https://github.com/haasn))

- Renamed `expandPath` to `scalePath` to make room [\#61](https://github.com/diagrams/diagrams-lib/pull/61) ([fryguybob](https://github.com/fryguybob))

- Envelope deformation [\#60](https://github.com/diagrams/diagrams-lib/pull/60) ([mgsloan](https://github.com/mgsloan))

- Added `Eq` instances for some data structures. [\#59](https://github.com/diagrams/diagrams-lib/pull/59) ([fryguybob](https://github.com/fryguybob))

- More involved diameter benchmark + better implementation [\#58](https://github.com/diagrams/diagrams-lib/pull/58) ([mgsloan](https://github.com/mgsloan))

- Add NullBackend for Ellipsoid [\#57](https://github.com/diagrams/diagrams-lib/pull/57) ([mgsloan](https://github.com/mgsloan))

- Fixed `arc` and `arcT` so they are always CCW. [\#56](https://github.com/diagrams/diagrams-lib/pull/56) ([fryguybob](https://github.com/fryguybob))

- new sized, sizedAs, and sizeSpec2D functions [\#52](https://github.com/diagrams/diagrams-lib/pull/52) ([byorgey](https://github.com/byorgey))

- Update to track with diagrams-core renaming, and a better Show instance for R2 [\#47](https://github.com/diagrams/diagrams-lib/pull/47) ([byorgey](https://github.com/byorgey))

- Minor tweaks due to monoid-extra and dual-tree changes [\#45](https://github.com/diagrams/diagrams-lib/pull/45) ([byorgey](https://github.com/byorgey))

- split out default attribute setting and size adjustment into separate functions [\#42](https://github.com/diagrams/diagrams-lib/pull/42) ([byorgey](https://github.com/byorgey))

- make beneath infixl 6 [\#41](https://github.com/diagrams/diagrams-lib/pull/41) ([byorgey](https://github.com/byorgey))

- scale invariant [\#3](https://github.com/diagrams/diagrams-lib/pull/3) ([ludflu](https://github.com/ludflu))

- BoundingBox is now Monoidal [\#2](https://github.com/diagrams/diagrams-lib/pull/2) ([mgsloan](https://github.com/mgsloan))

- New module enabling nice syntax for constructing and pattern-matching literal points and vectors [\#1](https://github.com/diagrams/diagrams-lib/pull/1) ([byorgey](https://github.com/byorgey))

## [v0.5.0.1](https://github.com/diagrams/diagrams-lib/tree/0_5_0_1) (2012-07-24)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/0_5...0_5_0_1)

## [v0.5](https://github.com/diagrams/diagrams-lib/tree/0_5) (2012-03-09)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/0_4...0_5)

**New features**
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

**Performance improvements**

- Use a balanced folding scheme for `cat'`, reducing time in some
  cases from \\(O(n^2)\\) to \\(O(n \\log n)\\)

- More efficient implementation of `beside`

**New instances**

- `Alignable` instances for `QDiagram`, `Path`, `Envelope`, `Active`, `Set`,
  `Map`, `[]`

- `Renderable` instances for `NullBackend` (`Trail`, `Path`, `Segment`,
  `Image`, `Text`)

- Instances for `Active`: `V`, `HasOrigin`, `Transformable`,
  `HasStyle`, `PathLike`, `Juxtaposable`, `Alignable`

**API changes**

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

**Dependency/version changes**

- Support for GHC 7.4.1:

    + depend on `colour` >= 2.3.2

    + update `base` and `array` upper bounds

- bump `vector-space` upper bound

**Bug fixes**

- Avoid scale by zero error in `showOrigin`.
- Base `adjustDia2D` translation on output size rather than diagram size.

## [v0.4.0.1] () (30 October 2011)
------------------------

- bump `data-default` dependency to allow version 0.3

## [v0.4]() (23 October 2011)
--------------------

**documentation fixes**

**New functions and primitives**

+ `wedge` shape primitive

+ `fromDirection` function for converting angles to 2D unit vectors;

  inverse function `direction` generalized to return any Angle type
+ New functions for computing and adjusting segment lengths

+ `scaleUToX` and `scaleUToY` for doing uniform scales
  resulting in a desired width or height.

+ `circlePath`, `reversePath`, `decoratePath`


**New features**

+ Completely new and improved polygon generation API

+ Cubic splines

+ User-controllable path fill rules

**Bug fixes**

+ fix incorrect corner case in arc generation

+ fix incorrect `reverseTrail` function

## [v0.3]() (18 June 2011)
-----------------

**New features**

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

**Bug fixes**

+ Issue 32 (`mempty` not behaving correctly within concatenations)

## [v0.2]() (3 June 2011)
----------------

**New functions and primitives**

+ `scaleToX` and `scaleToY` for scaling to an absolute width/height

+ `reverseTrail`

+ new `Angle` class and ability to use radians, degrees, or circle fractions
  for specifying angles

+ `rotateAbout` and `reflectAbout` transformations based on new conjugation functions

+ `rect` and `roundedRect` primitives

+ `explodeTrail`/`Path` for breaking trails and paths into individual segments

**New features**

+ opacity attribute

+ support for path clipping

* **New modules**
    + `Diagrams.BoundingBox`

**Fixes and updates**


+ `withBounds` now properly uses the new bounds instead of just combining
      them with the old

## [v0.1.1]() (18 May 2011)
------------------

* minor documentation fixes

* link to new website

## [v0.1]() [17 May 2011]
----------------

* initial preview release

\* *This Change Log was automatically generated by (and then edited by hand) [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
