## Test environments

* Mac x86_64-apple-darwin17.0, R 4.5.0

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## Downstream dependencies

None.

## Tests

Passes all the tests in `tests/testthat.R`

## New package

`BayesBrainMap` will be a new package on CRAN.

## Previous submisison (0.1.1)

  Possibly misspelled words in DESCRIPTION:
      ICA (28:5, 29:63, 33:30)
      al (28:70, 30:38)
      et (28:67, 30:35)
      neuroimaging (36:26)

> These words are correctly spelled.

  Suggests or Enhances not in mainstream repositories:
    INLA
  Availability using Additional_repositories specification:
    INLA   yes   https://inla.r-inla-download.org/R/testing

> INLA is a required package specified with Additional_repositories.

  Found the following (possibly) invalid file URI:
    URI: github.com/mandymejia/templateICAr
      From: README.md

> We have edited the URL to "https://github.com/mandymejia/templateICAr"

  Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-x86_64
  Check: code files for non-ASCII characters, Result: WARNING
    Found the following file with non-ASCII characters:
      R/estimate_prior.R
    Portable packages must use only ASCII characters in their R code and
    NAMESPACE directives, except perhaps in comments.
    Use \uxxxx escapes for other characters.
    Function 'tools::showNonASCIIfile' can help in finding non-ASCII
    characters in files.

> The non-ASCII character has been replaced.

  Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-x86_64
  Check: R code for possible problems, Result: NOTE
    estimate_prior: no visible global function definition for
      'complete.cases'
    Undefined global functions or variables:
      complete.cases
    Consider adding
      importFrom("stats", "complete.cases")
    to your NAMESPACE file.

> `importFrom("stats", "complete.cases")` has been added to the NAMESPACE.