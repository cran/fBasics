# fBasics 4041.97

- fixed the missing package qualifier in a link to another ackage.



# fBasics 4032.96

- modified `interactivePlot` to allow also functions in argument
  `plotFun`. Previously `plotFun` had to be a character vector containing the
  names of the functions. Adjusted the documentation and added an example to
  this effect.

- function `.distCheck` is defunct, use `distCheck` instead. To allow
  compatibility with older versions of packages that import fBasics (fGarch,
  fExtremes), `.distCheck` is exported but new code should not use it.

- removed generic `.print`, its 'ssd' method, and the S3 registration for the
  latter, all unused.

- clarified in the documentation for `histPlot`, `densityPlot` and
  `logDensityPlot` that argument `x` must be from class "timeSeries" (each of
  them starts with `stopifnot(is.timeSeries(x))`). Previously it was stated that
  argument `x` can be of any class that can be converted to "timeSeries" with
  `as.timeSeries`.

- contrary to the documentation, the return values from the \code{p/d/q/r*}
  functions (such as `dhyp`, `phyp`, `qhyp`) for the generalised distributions
  do not contain attribute \code{"param"}. This is now documented correctly.

- removed attribute 'control' from the return value of `rhyp` and similar
  functions. It was undocumented (or rather, the documentation stated that it is
  called "param"; in any case, wouldn't be usable in code).

- argument 'alpha' of `dhyp`, `phyp`, `qhyp` and `rhyp` can now be also a
  vector of length 4 containing the four parameters.

- `hypMode` now throws error if argument 'pm' doesn't have one of the allowed
  values.

- the return values of `*Mean()`, `*Var()`, `*Skew()`, `*Kurt()`, `*Moments()`,
  where '*' is a distribution prefix (such as 'gh' or 'ght'), are now all named.
  Previously, only some of them were.

- removed a number of inconsistencies in the documentation.

 
# fBasics 4031.95

- now `characterTable` doesn't try to print/plot invalid non-ASCII UTF8
  characters (fixes CRAN issue from around 2023-10-06).

- `symbolTable` now plots more characters and issues warnings only for
  characters in `26:31` which are ignored according to `?points` but are
  actually plotted.

- now using default R Random generators in tests, since the old ones were
  causing numerous warnings from tests.

- removed deprecated function `listDescription`, use `utils::packageDescription`
  instead.

- fixed a bug in `histPlot`, causing a puzzling error when argument
  `fit = FALSE`.


# fBasics 4022.94

- `dagotest()` was returning NaN for the value of the test statistic based on
  kurtosis when the argument of a cube root was negative. Reported by Cameron
  Willden who suggested the fix.

- the name of the first column of dataset `msft.dat` is not mangled any more (it
  was "X.Y..m..d" and now is "%Y-%m-%d", as in the input file), the rest of the
  dataset is as before. The file from which the dataset is created is now taken
  from an identical file in package `timeSeries`.

- the `show` methods for classes `fHTEST` and `fDISTFIT` no longer print
  'Description:' when slot `description = ""`.

- many fitting functions, e.g., `nFit`, were inadvertently ignoring argument
  `description` by using `description = description()` instead of
  `description = description` in the call to `new` when creating the return
  value.

- `correlationTest`, `pearsonTest`, `spearmanTest`, `kendallTest`, `ks2Test`,
  `scaleTest`, `varianceTest`, and `locationTest` now set slot `description` of
  the result to `""` by default. Previously they were setting it to the current
  date/time, with the consequence that the functions gave different objects at
  each run. Use `description = date()` in the call if you want the
  date. Similarly for `ksnormTest`, `shapiroTest`, `jarqueberaTest`, `dagoTest`,
  `normalTest`, `adTest`, `cvmTest`, `lillieTest`, `pchiTest`, `sfTest`,
  `jbTest`, `nFit`, `tFit`, `stableFit`, and many others, which had default
  setting `description = description()`.


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

