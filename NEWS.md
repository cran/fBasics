# fBasics 4021.93

- fixed CRAN warnings about a function declaration without a prototype, which is
  deprecated by modern C compilers.

- further updates and improvements to the documentation.

- website built with `pkgdown` is now linked to in file `DESCRIPTION`.


# fBasics 4021.92

- new maintainer: Georgi N. Boshnakov.

- in DESCRIPTION, moved `timeDate` and `timeSeries` from `Depends:` to
  `Imports:`. This necessitated changes in the examples and unit tests to get
  them run smoothly. The implications for end users and developers are discussed
  in separate sections below.

- in NAMESPACE, now export selectively rather than with a generic pattern.  No
  longer export symbols starting with a dot. Reexported some functions that can
  reasonably be expected when `fBasics` is attached (the list of such functions
  can be adjusted).
  
- tidied up the documentation somewhat. There is now a `_pkgdown` file with the
  functions in the package organised by topic. Run `pkgdown::build_site()` on
  the source directory (or unpacked tarball) to build the site locally.

- removed `.HedgeFund1` and `.HedgeFund2` - they were just used to build the
  dataset `HedgeFund`.

## Notes for users

The changes are aimed at making the package easier to manage and reduce the side
effects for users from attaching it.

- Since packages `timeDate` and `timeSeries` are no longer attached by
  `library(fBasics)`, users may need to attach them explicitly, if they use
  functions from them directly. That said, some functions from `timeDate` and
  `timeSeries` are reexported by `fBasics`, so most users may not even notice
  any change.

- Undocumented functions (mostly starting with a dot, '.') are no longer
  exported. If you believe that a non-exported and undocumented function should
  be exported, please open a bug report, giving your reasons.


## Notes for developers

  The notes for users apply to developers, as well. In addition:

- We continue to export some (undocumented) functions starting with a '.' to
  avoid breaking packages that import `fBasics`. Please consider using
  documented equivalents, if possible. Alternatively, let us know that they are
  useful, so that we can document them. 





# fBasics 3042.89.2 and older versions

  See file `ChangeLog` for changes before 4021.92.

