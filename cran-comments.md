## Resubmission

This is a resubmission. We thank the CRAN reviewer (Leonore Hochhauser) and
have addressed all the points raised:

* Removed the single quotes around the function names in the Description field.
* Added references describing the methods to the Description field, in the
  form authors (year) <doi:...>.
* Rewrote the examples of cumba_experiment() and cumba_scenario() so that they
  are directly executable on the bundled dataset and run in < 5 seconds.
  All \dontrun{} wrappers and all commented-out example lines were removed.
* Console output is no longer written unconditionally: both exported functions
  now take a 'verbose' argument (default FALSE), and every cat() call is issued
  only when verbose = TRUE, so messages can be fully suppressed.

## Test environments

* Local: Windows 11 x64, R 4.4.2 (`R CMD check --as-cran`)
* win-builder: R-devel and R-release

## R CMD check results

0 errors | 0 warnings | 1 note

The remaining NOTE is the standard "New submission". "Brix" and "phenology"
flagged as possibly misspelled are correct domain terms.

## Downstream dependencies

This is a new submission, so there are no reverse dependencies.
