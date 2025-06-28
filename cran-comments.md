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

## Previous submisison (0.1.2)

  \dontrun{} should only be used if the example really cannot be executed
  (e.g. because of missing additional software, missing API keys, ...) by
  the user. That's why wrapping examples in \dontrun{} adds the comment
  ("# Not run:") as a warning for the user. Please replace \dontrun with
  \donttest.

  Please unwrap the examples if they are executable in < 5 sec, or replace
  dontrun{} with \donttest{}.

  Please wrap examples that need packages in ‘Suggests’ in
  if(requireNamespace("pkgname")){} instead.

  For more details:
  <https://contributor.r-project.org/cran-cookbook/general_issues.html#structuring-of-examples>

> The examples wrapped with \dontrun{} really cannot be executed by users, because they require input data which is not possible to provide. These examples are included to help users understand the correspondance between the expected neuroimaging data files and the function arguments. They cannot be converted to \donttest{} because the required input data are too large for practical inclusion. Creating a smaller example would not meaningfully demonstrate the functions’ intended use.

  You write information messages to the console that cannot be easily
  suppressed.
  It is more R like to generate objects that can be used to extract the
  information a user is interested in, and then print() that object.
  Instead of print()/cat() rather use message()/warning() or
  if(verbose)cat(..) (or maybe stop()) if you really have to write text to
  the console. (except for print, summary, interactive functions) ->
  R/BrainMap.methods.R; R/BrainMap.R; R/dual_reg2.R; R/EM_BrainMap.R;
  R/engagements.methods.R; R/engagements.R; R/estimate_prior.methods.R;
  R/estimate_prior.R; R/resample_prior.R; R/rm_nuisIC.R; R/utils.R;
  R/VB_FCBrainMap.R
  For more details:
  <https://contributor.r-project.org/cran-cookbook/code_issues.html#using-printcat>

> All print()/cat() have been wrapped by verbose() or converted to message(), except those inside print and summary functions.

  Please ensure that you do not use more than 2 cores in your examples,
  vignettes, etc.
  For more details:
  <https://contributor.r-project.org/cran-cookbook/code_issues.html#using-more-than-2-cores>

> In the examples for applicable functions, the argument `usePar=FALSE` has been added.

  Please do not set a seed to a specific number within a function.
  For more details:
  <https://contributor.r-project.org/cran-cookbook/code_issues.html#setting-a-specific-seed>

> `seed=1234` has been added as an argument to the relevant functions. Users can now set `seed=NULL` to avoid changing the seed.