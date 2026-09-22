## Resubmission

This resubmission adds small executable examples for every exported function,
as requested by CRAN. The examples use simulated data, write temporary output
only to tempdir(), and are run automatically by R CMD check.

## Test environments

* Local: Windows 10 x64, R 4.6.0
* GitHub Actions:
  * Windows, R release
  * macOS, R release
  * Ubuntu, R release
  * Ubuntu, R-devel
* win-builder: Windows Server 2022 x64, R-devel

## R CMD check results

The package is checked with 0 errors and 0 warnings. The only incoming NOTE
identifies this as a new submission and flags technical terms and cited author
surnames.

## Notes

"AICc" is the standard abbreviation for the small-sample corrected Akaike
information criterion. Burnham, Nakagawa, and Schielzeth are surnames in the
methodological references included in DESCRIPTION.

## Downstream dependencies

There are currently no downstream dependencies.
