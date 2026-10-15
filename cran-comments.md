## Submission

This is a new submission of the `cumba` package (version 0.1.0).

## Test environments

* Local: Windows 11 x64, R 4.4.2 (`devtools::check()`, `--as-cran`)
* win-builder: R-devel (2026-10-14 r90673) and R-release

## R CMD check results

0 errors | 0 warnings | 1 note

On win-builder the only NOTE comes from "checking CRAN incoming feasibility":

* New submission.
  This is the first submission of `cumba` to CRAN.

* Possibly misspelled words in DESCRIPTION: "Brix", "phenology".
  These are correct domain terms: "Brix" is the standard unit of total
  soluble solids content in fruit, and "phenology" is the study of crop
  developmental stages.

(The local check additionally reported a "future file timestamps / unable to
verify current time" NOTE; this is environmental and does not occur on
win-builder.)

## Downstream dependencies

This is a new submission, so there are no reverse dependencies.
