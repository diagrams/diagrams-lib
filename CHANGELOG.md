# Change Log

## [v1.3]() (2015-04-03)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.2.0.8...v1.2.0.9)

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

* **Dependency/version changes**
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

- update for GHC-7.10, -Wall [\#238](https://github.com/diagrams/diagrams-lib/pull/238) ([bergey](https://github.com/bergey))

- Non lenses [\#202](https://github.com/diagrams/diagrams-lib/pull/202) ([bergey](https://github.com/bergey))

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

- Strokable class [\#224](https://github.com/diagrams/diagrams-lib/pull/224) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Bump lens upper version bounds [\#223](https://github.com/diagrams/diagrams-lib/pull/223) ([RyanGlScott](https://github.com/RyanGlScott))

- Generalize double [\#191](https://github.com/diagrams/diagrams-lib/pull/191) ([Mathnerd314](https://github.com/Mathnerd314))

## [v1.2.0.4](https://github.com/diagrams/diagrams-lib/tree/v1.2.0.4) (2014-10-08)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.2.0.3...v1.2.0.4)

**Merged pull requests:**

- add semi-portable looping using fsnotify [\#213](https://github.com/diagrams/diagrams-lib/pull/213) ([bergey](https://github.com/bergey))

- Port to linear [\#212](https://github.com/diagrams/diagrams-lib/pull/212) ([cchalmers](https://github.com/cchalmers))

- A potential solution to diagrams/diagrams-cairo\#50. [\#195](https://github.com/diagrams/diagrams-lib/pull/195) ([byorgey](https://github.com/byorgey))

- dynamic typing [\#183](https://github.com/diagrams/diagrams-lib/pull/183) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

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

- arrow head size specify in terms of width instead of radius [\#184](https://github.com/diagrams/diagrams-lib/pull/184) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- No mco [\#175](https://github.com/diagrams/diagrams-lib/pull/175) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v1.1.0.4](https://github.com/diagrams/diagrams-lib/tree/v1.1.0.4) (2014-04-04)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.1.0.2...v1.1.0.4)

**Merged pull requests:**

- Updates to work with `Backend` redesign [\#171](https://github.com/diagrams/diagrams-lib/pull/171) ([byorgey](https://github.com/byorgey))

- Rework of units [\#169](https://github.com/diagrams/diagrams-lib/pull/169) ([byorgey](https://github.com/byorgey))

- Make Diagrams.Transform.under more polymorphic [\#168](https://github.com/diagrams/diagrams-lib/pull/168) ([FlorentBecker](https://github.com/FlorentBecker))

## [v1.1.0.2](https://github.com/diagrams/diagrams-lib/tree/v1.1.0.2) (2014-03-19)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.1.0.1...v1.1.0.2)

**Merged pull requests:**

- move avgScale to core [\#167](https://github.com/diagrams/diagrams-lib/pull/167) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- 3d color & lighting, more solids, Angle utilities [\#166](https://github.com/diagrams/diagrams-lib/pull/166) ([bergey](https://github.com/bergey))

- Added bothSize function, lineHead and lineTail [\#165](https://github.com/diagrams/diagrams-lib/pull/165) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v1.1.0.1](https://github.com/diagrams/diagrams-lib/tree/v1.1.0.1) (2014-03-09)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.1...v1.1.0.1)

## [v1.1](https://github.com/diagrams/diagrams-lib/tree/v1.1) (2014-03-09)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v1.0.1...v1.1)

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

- SVG backend doesn't fill loops if they occur in the same subtree as a line [\#141](https://github.com/diagrams/diagrams-lib/pull/141) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

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

**Fixed bugs:**

- Example where offsetTrail does not work as expected [\#118](https://github.com/diagrams/diagrams-lib/issues/118)

- Arrows do not behave correctly under scaling [\#112](https://github.com/diagrams/diagrams-lib/issues/112)

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

- very basic color scheme chooser [\#139](https://github.com/diagrams/diagrams-lib/pull/139) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Make cat' and friends accumulate seperation between empty diagrams [\#130](https://github.com/diagrams/diagrams-lib/pull/130) ([bergey](https://github.com/bergey))

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

- Align works with both envelope and trace [\#101](https://github.com/diagrams/diagrams-lib/pull/101) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v0.7](https://github.com/diagrams/diagrams-lib/tree/v0.7) (2013-08-09)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/v0.6.0.3...v0.7)

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

- Functions for asymmetric padding + dep bumps ghc7.6 + misc fixes [\#40](https://github.com/diagrams/diagrams-lib/pull/40) ([mgsloan](https://github.com/mgsloan))

## [0_5_0_1](https://github.com/diagrams/diagrams-lib/tree/0_5_0_1) (2012-07-24)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/0_5...0_5_0_1)

## [0_5](https://github.com/diagrams/diagrams-lib/tree/0_5) (2012-03-09)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/0_4...0_5)

## [0_4](https://github.com/diagrams/diagrams-lib/tree/0_4) (2011-10-23)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/0_3...0_4)

## [0_3](https://github.com/diagrams/diagrams-lib/tree/0_3) (2011-06-18)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/0_2...0_3)

## [0_2](https://github.com/diagrams/diagrams-lib/tree/0_2) (2011-06-03)

[Full Changelog](https://github.com/diagrams/diagrams-lib/compare/0_1...0_2)

## [0_1](https://github.com/diagrams/diagrams-lib/tree/0_1) (2011-05-16)



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
