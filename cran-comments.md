## Resubmission

This is a resubmission addressing feedback from CRAN volunteers. In this version I have:

* Reduced package size from 6.2 MB to 1.2 MB by excluding the `inst/extdata` directory (containing a 7 MB Excel file used only for data processing during development)
* Added a reference to the Eurostat NUTS methodology in the Description field
* Added executable examples to all exported functions (`match_city()`, `normalize_city()`, `generate_fake_cities()`)
* Updated version number to 0.2.1

## Test environments

* local macOS install, R 4.2.3
* R CMD check results: 0 errors | 2 warnings | 4 notes

## R CMD check results

There were no ERRORs.

There were 2 WARNINGs:

* checking R files for non-ASCII characters: Non-ASCII characters appear in example code for `normalize_city()` and within `generate_fake_cities()`. These represent real-world international city names (e.g., München, Zürich) which are the package's primary use case.

* checking for missing documentation entries: Internal data objects (`lau_*` and `nuts_*`) are not exported and are used internally by the package functions. These are lookup tables and do not require user-facing documentation.

There were 4 NOTEs:

* checking for future file timestamps: Unable to verify current time (testing environment issue)

* checking DESCRIPTION meta-information: License components which are templates and need '+ file LICENSE': MIT

* checking top-level files: LICENSE file is not mentioned in the DESCRIPTION file

* checking dependencies in R code: Some declared Imports are not explicitly imported via namespace. These packages are used by internal functions and data processing.

## Downstream dependencies

There are currently no downstream dependencies for this package.
