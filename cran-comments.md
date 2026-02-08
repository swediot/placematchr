## Resubmission

This is a resubmission addressing the issues found during the incoming checks for version 0.2.1.
In this version (0.2.2) I have:

* Replaced all non-ASCII characters in R code with Unicode escapes to fix "Non-ASCII characters" warnings.
* Added documentation for all exported data objects (`lau_*`, `nuts_*`) to fix "Undocumented code objects" warnings.
* Corrected test expectations in `tests/testthat/test-normalization.R` to fix test failures.
* Updated `DESCRIPTION` to include `MIT + file LICENSE`.
* Added `cran-comments.md` to `.Rbuildignore`.

## Test environments

* local macOS install, R 4.4.0
* R CMD check results: 0 errors | 0 warnings | 1 notes

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking DESCRIPTION meta-information ... NOTE
  Possibly misspelled words in DESCRIPTION:
    Eurostat
    LAU
  
  These are correct acronyms/names in the context of European statistics.

## Downstream dependencies

There are currently no downstream dependencies for this package.
